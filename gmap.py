################################################
import pygame
import sys, time, random, heapq
from pyswip import Prolog, Functor, Variable, Query
from TreeNode import TreeNode

import pathlib
current_path = str(pathlib.Path().resolve())

elapsed_time = 0
auto_play_tempo = 0.005
auto_play = True  # desligar para controlar manualmente
show_map = False

scale = 60
size_x = 12
size_y = 12
width = size_x * scale  #Largura Janela
height = size_y * scale #Altura Janela

player_pos = (1,1,'norte')
energia = 0
pontuacao = 0


mapa=[['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','','']]

visitados = []
certezas = []

pl_file = (current_path + '\\main.pl').replace('\\','/')
prolog = Prolog()
prolog.consult(pl_file)

last_action = ""

action_queue = []

def heuristic(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def get_neighbors(pos):
    x, y = pos
    neighbors = []
    # Check boundaries (1 to 12)
    if x > 1: neighbors.append((x-1, y))
    if x < 12: neighbors.append((x+1, y))
    if y > 1: neighbors.append((x, y-1))
    if y < 12: neighbors.append((x, y+1))
    return neighbors

def a_star(start, goal):
    # start is (x, y, direction)
    # goal is (x, y)
    # Returns list of actions
    
    start_node = TreeNode(start, 0, 0)
    open_set = []
    heapq.heappush(open_set, start_node)
    visited = set()
    
    # Map to store best node for each coordinate/direction to reconstruct path
    came_from = {}
    
    while open_set:
        current = heapq.heappop(open_set)
        curr_pos = current.get_coord() # (x, y, dir)
        
        if (curr_pos[0], curr_pos[1]) == goal:
            # Reconstruct path
            path = []
            while current.get_parent():
                # Determine action from parent to current
                parent_pos = current.get_parent().get_coord()
                # Logic to determine action... wait, A* usually returns nodes. 
                # We need actions. Let's store action in the node or deduce it.
                # Actually, let's deduce it.
                # But simpler: The transitions define the actions.
                # Let's store the action taken to reach this node in the node itself?
                # TreeNode doesn't have action field. We can subclass or just use a wrapper.
                # Or just deduce:
                # If pos changed, it was 'andar'.
                # If dir changed, it was 'virar_...'.
                path.append(current)
                current = current.get_parent()
            path.reverse()
            
            actions = []
            # We need to extract actions from the path of states
            # path[0] is the first step AFTER start.
            prev = start
            for node in path:
                curr = node.get_coord()
                if curr[0] != prev[0] or curr[1] != prev[1]:
                    actions.append('andar')
                elif curr[2] != prev[2]:
                    # Rotation
                    dirs = ['norte', 'leste', 'sul', 'oeste']
                    idx_prev = dirs.index(prev[2])
                    idx_curr = dirs.index(curr[2])
                    diff = (idx_curr - idx_prev) % 4
                    if diff == 1: actions.append('virar_direita')
                    elif diff == 3: actions.append('virar_esquerda')
                    # 180 turn is 2 turns, but our neighbors logic should handle single turns
                prev = curr
            
            # Log do custo total do caminho
            total_cost = path[-1].get_value_gx() if path else 0
            if total_cost > 100:
                print(f"[A*] ⚠️  ATENÇÃO: Caminho com RISCO! Custo total: {total_cost}")
                if total_cost > 1000:
                    print(f"[A*] 🔴 CAMINHO PASSA POR BURACO! Chance de morte!")
            
            return (actions, total_cost)  # Retorna tupla com ações E custo

        if (curr_pos[0], curr_pos[1], curr_pos[2]) in visited:
            continue
        visited.add((curr_pos[0], curr_pos[1], curr_pos[2]))
        
        # Generate successors (Actions: Andar, Virar Esquerda, Virar Direita)
        # 1. Andar
        x, y, d = curr_pos
        nx, ny = x, y
        if d == 'norte': ny += 1
        elif d == 'sul': ny -= 1
        elif d == 'leste': nx += 1
        elif d == 'oeste': nx -= 1
        
        # Check if move is valid (in bounds)
        if 1 <= nx <= 12 and 1 <= ny <= 12:
             # Base cost of move
             move_cost = 1
             
             # Consulta Prolog para verificar riscos da célula (nx, ny)
             # Penaliza células perigosas para que A* evite elas
             try:
                 # Verifica se célula já foi visitada (segura)
                 is_visited = list(prolog.query(f"visitado({nx},{ny})"))
                 
                 if not is_visited:
                     # Verifica memory para perigos
                     memory_query = list(prolog.query(f"memory({nx},{ny},M)"))
                     
                     if memory_query:
                         dangers = memory_query[0]['M']
                         
                         # PENALIDADES POR TIPO DE RISCO
                         # PENALIDADES POR TIPO DE RISCO
                         if 'brisa' in dangers:
                             # Verifica se é BURACO CONFIRMADO
                             is_confirmed = list(prolog.query(f"certeza({nx},{ny})"))
                             if is_confirmed:
                                 move_cost += 2000  # BURACO CONFIRMADO = morte certa
                             else:
                                 move_cost += 100   # SUSPEITA DE BURACO = risco, mas passável
                                 
                         elif 'passos' in dangers:
                             # MONSTRO CONFIRMADO (já passou e tomou dano)
                             # Custo MUITO ALTO para evitar passar de novo!
                             # O usuário pediu peso "absurdamente maior" que "talvez buraco" (1000)
                             move_cost += 5000
                         elif 'palmas' in dangers or 'flash' in dangers:
                             move_cost += 50    # Teleportador = incerteza, penaliza moderado
                 else:
                     # Célula foi VISITADA - verifica se tem monstro CONFIRMADO
                     memory_query = list(prolog.query(f"memory({nx},{ny},M)"))
                     if memory_query:
                         dangers = memory_query[0]['M']
                         if 'passos' in dangers:
                             # MONSTRO CONFIRMADO (já passou e tomou dano)
                             move_cost += 5000
             except:
                 pass  # Se consulta falhar, usa custo base
             
             new_g = current.get_value_gx() + move_cost
             new_h = heuristic((nx, ny), goal)
             new_node = TreeNode((nx, ny, d), new_g + new_h, new_g)
             new_node.set_parent(current)
             heapq.heappush(open_set, new_node)

        # 2. Virar Direita
        dirs = ['norte', 'leste', 'sul', 'oeste']
        idx = dirs.index(d)
        new_d = dirs[(idx + 1) % 4]
        new_node = TreeNode((x, y, new_d), current.get_value_gx() + 1 + heuristic((x,y), goal), current.get_value_gx() + 1)
        new_node.set_parent(current)
        heapq.heappush(open_set, new_node)

        # 3. Virar Esquerda
        new_d = dirs[(idx - 1) % 4]
        new_node = TreeNode((x, y, new_d), current.get_value_gx() + 1 + heuristic((x,y), goal), current.get_value_gx() + 1)
        new_node.set_parent(current)
        heapq.heappush(open_set, new_node)
        
    return ([], 0)  # No path found - retorna tupla vazia

def decisao():
    global action_queue, last_action
    
    if action_queue:
        action = action_queue.pop(0)
        print(f"[AÇÃO] Executando da fila: {action}")
        return action

    # Query Prolog for high-level objective
    solucoes = list(prolog.query("proximo_objetivo(X, Y, Acao)"))
    
    if not solucoes:
        print("[ERRO] Prolog não retornou soluções!")
        return ""
        
    sol = solucoes[0]
    acao_prolog = str(sol['Acao'])
    target_x = sol['X']
    target_y = sol['Y']
    
    print(f"[DECISÃO] Prolog diz: {acao_prolog} para ({target_x}, {target_y})")
    
    if acao_prolog == 'pegar':
        print(f"[AÇÃO] Pegando item em ({target_x}, {target_y})")
        return 'pegar'
    
    if acao_prolog == 'sair':
        print("="*50)
        print("[FIM] AGENTE DECIDIU SAIR. FIM DE JOGO.")
        print("="*50)
        global last_action, auto_play
        last_action = "SAIU"
        auto_play = False
        return ""

    if acao_prolog == 'ir_para':
        # Plan path to (target_x, target_y)
        start_pos = (player_pos[0], player_pos[1], player_pos[2])
        goal_pos = (target_x, target_y)
        
        print(f"[NAVEGAÇÃO] De {start_pos} para {goal_pos}")
        
        if (start_pos[0], start_pos[1]) == goal_pos:
            print("[INFO] Já está no alvo")
            return ""
            
        result = a_star(start_pos, goal_pos)
        actions, path_cost = result if result else ([], 0)
        
        if actions:
            print(f"[A*] Caminho encontrado: {actions}")
            action_queue.extend(actions)
            return action_queue.pop(0)
        else:
            print(f"[ERRO] A* não encontrou nenhum caminho para ({target_x}, {target_y})!")
            return "virar_direita"

    return ""


def exec_prolog(a):
    global last_action
    if a != "":
        list(prolog.query(a))
    last_action = a

def update_prolog():
    global player_pos, mapa, energia, pontuacao,visitados, show_map

    list(prolog.query("atualiza_obs, verifica_player"))

    x = Variable()
    y = Variable()
    visitado = Functor("visitado", 2)
    visitado_query = Query(visitado(x,y))
    visitados.clear()
    while visitado_query.nextSolution():
        visitados.append((x.value,y.value))
    visitado_query.closeQuery()

    x = Variable()
    y = Variable()
    certeza = Functor("certeza", 2)
    certeza_query = Query(certeza(x,y))
    certezas.clear()
    while certeza_query.nextSolution():
        certezas.append((x.value,y.value))
    certeza_query.closeQuery()
        
    if show_map:    
        x = Variable()
        y = Variable()
        z = Variable()    
        tile = Functor("tile", 3)
        tile_query = Query(tile(x,y,z))
        while tile_query.nextSolution():
            mapa[y.get_value()-1][x.get_value()-1] = str(z.value)
        tile_query.closeQuery()

    else:

        y = 0
        for j in mapa:
            x = 0
            for i in j:
                mapa[y][x] = ''
                x  += 1
            y +=  1

        x = Variable()
        y = Variable()
        z = Variable()    
        memory = Functor("memory", 3)
        memory_query = Query(memory(x,y,z))
        while memory_query.nextSolution():
            for s in z.value:
                
                if str(s) == 'brisa':
                    mapa[y.get_value()-1][x.get_value()-1] += 'P'
                elif str(s) == 'palmas':
                    mapa[y.get_value()-1][x.get_value()-1] += 'T'
                elif str(s) == 'passos':
                    mapa[y.get_value()-1][x.get_value()-1] += 'D'
                elif str(s) == 'reflexo':
                    mapa[y.get_value()-1][x.get_value()-1] += 'U'
                elif str(s) == 'brilho':
                    mapa[y.get_value()-1][x.get_value()-1] += 'O'
            
        memory_query.closeQuery()

    x = Variable()
    y = Variable()
    z = Variable()

    posicao = Functor("posicao", 3)
    position_query = Query(posicao(x,y,z))
    position_query.nextSolution()
    player_pos = (x.value,y.value,str(z.value))
    position_query.closeQuery()

    x = Variable()
    energia = Functor("energia", 1)
    energia_query = Query(energia(x))
    energia_query.nextSolution()
    energia = x.value
    energia_query.closeQuery()

    x = Variable()
    pontuacao = Functor("pontuacao", 1)
    pontuacao_query = Query(pontuacao(x))
    pontuacao_query.nextSolution()
    pontuacao = x.value
    pontuacao_query.closeQuery()

    #print(mapa)
    #print(player_pos)


def load():
    global sys_font, clock, img_wall, img_grass, img_start, img_finish, img_path
    global img_gold,img_health, img_pit, img_bat, img_enemy1, img_enemy2,img_floor
    global bw_img_gold,bw_img_health, bw_img_pit, bw_img_bat, bw_img_enemy1, bw_img_enemy2,bw_img_floor
    global img_player_up, img_player_down, img_player_left, img_player_right, img_tomb

    sys_font = pygame.font.Font(pygame.font.get_default_font(), 20)
    clock = pygame.time.Clock() 

    img_wall = pygame.image.load('wall.jpg')
    #img_wall2_size = (img_wall.get_width()/map_width, img_wall.get_height()/map_height)
    img_wall_size = (width/size_x, height/size_y)
    
    img_wall = pygame.transform.scale(img_wall, img_wall_size)

    
    img_player_up = pygame.image.load('player_up.png')
    img_player_up_size = (width/size_x, height/size_y)
    img_player_up = pygame.transform.scale(img_player_up, img_player_up_size)

    img_player_down = pygame.image.load('player_down.png')
    img_player_down_size = (width/size_x, height/size_y)
    img_player_down = pygame.transform.scale(img_player_down, img_player_down_size)

    img_player_left = pygame.image.load('player_left.png')
    img_player_left_size = (width/size_x, height/size_y)
    img_player_left = pygame.transform.scale(img_player_left, img_player_left_size)

    img_player_right = pygame.image.load('player_right.png')
    img_player_right_size = (width/size_x, height/size_y)
    img_player_right = pygame.transform.scale(img_player_right, img_player_right_size)


    img_tomb = pygame.image.load('tombstone.png')
    img_tomb_size = (width/size_x, height/size_y)
    img_tomb = pygame.transform.scale(img_tomb, img_tomb_size)



    img_grass = pygame.image.load('grass.jpg')
    img_grass_size = (width/size_x, height/size_y)
    img_grass = pygame.transform.scale(img_grass, img_grass_size)

    img_floor = pygame.image.load('floor.png')
    img_floor_size = (width/size_x, height/size_y)
    img_floor = pygame.transform.scale(img_floor, img_floor_size)

    img_gold = pygame.image.load('gold.png')
    img_gold_size = (width/size_x, height/size_y)
    img_gold = pygame.transform.scale(img_gold, img_gold_size)

    img_pit = pygame.image.load('pit.png')
    img_pit_size = (width/size_x, height/size_y)
    img_pit = pygame.transform.scale(img_pit, img_pit_size)

    img_enemy1 = pygame.image.load('enemy1.png')
    img_enemy1_size = (width/size_x, height/size_y)
    img_enemy1 = pygame.transform.scale(img_enemy1, img_enemy1_size)

    img_enemy2 = pygame.image.load('enemy2.png')
    img_enemy2_size = (width/size_x, height/size_y)
    img_enemy2 = pygame.transform.scale(img_enemy2, img_enemy2_size)

    img_bat = pygame.image.load('bat.png')
    img_bat_size = (width/size_x, height/size_y)
    img_bat = pygame.transform.scale(img_bat, img_bat_size)

    img_health = pygame.image.load('health.png')
    img_health_size = (width/size_x, height/size_y)
    img_health = pygame.transform.scale(img_health, img_health_size)    
    
    bw_img_floor = pygame.image.load('bw_floor.png')
    bw_img_floor_size = (width/size_x, height/size_y)
    bw_img_floor = pygame.transform.scale(bw_img_floor, bw_img_floor_size)

    bw_img_gold = pygame.image.load('bw_gold.png')
    bw_img_gold_size = (width/size_x, height/size_y)
    bw_img_gold = pygame.transform.scale(bw_img_gold, bw_img_gold_size)

    bw_img_pit = pygame.image.load('bw_pit.png')
    bw_img_pit_size = (width/size_x, height/size_y)
    bw_img_pit = pygame.transform.scale(bw_img_pit, bw_img_pit_size)

    bw_img_enemy1 = pygame.image.load('bw_enemy1.png')
    bw_img_enemy1_size = (width/size_x, height/size_y)
    bw_img_enemy1 = pygame.transform.scale(bw_img_enemy1, bw_img_enemy1_size)

    bw_img_enemy2 = pygame.image.load('bw_enemy2.png')
    bw_img_enemy2_size = (width/size_x, height/size_y)
    bw_img_enemy2 = pygame.transform.scale(bw_img_enemy2, bw_img_enemy2_size)

    bw_img_bat = pygame.image.load('bw_bat.png')
    bw_img_bat_size = (width/size_x, height/size_y)
    bw_img_bat = pygame.transform.scale(bw_img_bat, bw_img_bat_size)

    bw_img_health = pygame.image.load('bw_health.png')
    bw_img_health_size = (width/size_x, height/size_y)
    bw_img_health = pygame.transform.scale(bw_img_health, bw_img_health_size)  

def update(dt, screen):
    
    global elapsed_time
    
    elapsed_time += dt
    
    if (elapsed_time / 1000) > auto_play_tempo:
        
        if auto_play and player_pos[2] != 'morto':
            # Capture pos before move
            prev_x, prev_y = player_pos[0], player_pos[1]
            
            action = decisao()
            exec_prolog(action)
            update_prolog()
            
            # Check for teleport
            curr_x, curr_y = player_pos[0], player_pos[1]
            dist = abs(curr_x - prev_x) + abs(curr_y - prev_y)
            
            if action == 'andar' and dist > 1:
                 print(f"[TELEPORT DETECTED] Jumped from ({prev_x},{prev_y}) to ({curr_x},{curr_y}). Recalculating route...")
                 action_queue.clear()
       
        elapsed_time = 0
        
    

def key_pressed(event):
    
    global show_map
    #leitura do teclado
    if event.type == pygame.KEYDOWN:
        
        if not auto_play and player_pos[2] != 'morto':
            if event.key == pygame.K_LEFT: #tecla esquerda
                exec_prolog("virar_esquerda")
                update_prolog()

            elif event.key == pygame.K_RIGHT: #tecla direita
                exec_prolog("virar_direita")
                update_prolog()

            elif event.key == pygame.K_UP: #tecla  cima
                exec_prolog("andar")
                update_prolog()

            if event.key == pygame.K_SPACE:
                exec_prolog("pegar")
                update_prolog()
    
        if event.key == pygame.K_m:
            show_map = not show_map
            update_prolog()


def draw_screen(screen):
    
    screen.fill((0,0,0))
 
    y = 0
    for j in mapa:
        x = 0
        for i in j:

            if (x+1,12-y) in visitados:
                screen.blit(img_floor, (x * img_floor.get_width(), y * img_floor.get_height()))
            else:
                screen.blit(bw_img_floor, (x * bw_img_floor.get_width(), y * bw_img_floor.get_height()))

            if mapa[11-y][x].find('P') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_pit, (x * img_pit.get_width(), y * img_pit.get_height()))                            
                else:
                    screen.blit(bw_img_pit, (x * bw_img_pit.get_width(), y * bw_img_pit.get_height()))                            

            if mapa[11-y][x].find('T') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_bat, (x * img_bat.get_width(), y * img_bat.get_height()))
                else:
                    screen.blit(bw_img_bat, (x * bw_img_bat.get_width(), y * bw_img_bat.get_height()))

            if mapa[11-y][x].find('D') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_enemy1, (x * img_enemy1.get_width(), y * img_enemy1.get_height()))                                               
                else:
                    screen.blit(bw_img_enemy1, (x * bw_img_enemy1.get_width(), y * bw_img_enemy1.get_height()))                                               
                            
            if mapa[11-y][x].find('d') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_enemy2, (x * img_enemy2.get_width(), y * img_enemy2.get_height()))                                               
                else:
                    screen.blit(bw_img_enemy2, (x * bw_img_enemy2.get_width(), y * bw_img_enemy2.get_height()))                                               

            if mapa[11-y][x].find('U') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_health, (x * img_health.get_width(), y * img_health.get_height()))                               
                else:
                    screen.blit(bw_img_health, (x * bw_img_health.get_width(), y * bw_img_health.get_height()))                               

            if mapa[11-y][x].find('O') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_gold, (x * img_gold.get_width(), y * img_gold.get_height()))                
                else:
                    screen.blit(bw_img_gold, (x * bw_img_gold.get_width(), y * bw_img_gold.get_height()))                
            
            if x == player_pos[0] - 1  and  y == 12 - player_pos[1]:
                if player_pos[2] == 'norte':
                    screen.blit(img_player_up, (x * img_player_up.get_width(), y * img_player_up.get_height()))                                               
                elif player_pos[2] == 'sul':
                    screen.blit(img_player_down, (x * img_player_down.get_width(), y * img_player_down.get_height()))                                               
                elif player_pos[2] == 'leste':
                    screen.blit(img_player_right, (x * img_player_right.get_width(), y * img_player_right.get_height()))                                               
                elif player_pos[2] == 'oeste':
                    screen.blit(img_player_left, (x * img_player_left.get_width(), y * img_player_left.get_height()))                                                                                                           
                else:
                    screen.blit(img_tomb, (x * img_tomb.get_width(), y * img_tomb.get_height()))                                                                                                           
            x  += 1
        y +=  1

    t = sys_font.render("Pontuação: " + str(pontuacao), False, (255,255,255))
    screen.blit(t, t.get_rect(top = height + 5, left=40))

    t = sys_font.render(last_action, False, (255,255,255))
    screen.blit(t, t.get_rect(top = height + 5, left=width/2-40))
    
    t = sys_font.render("Energia: " + str(energia), False, (255,255,255))
    screen.blit(t, t.get_rect(top = height + 5, left=width-140))

def main_loop(screen):  
    global clock
    running = True
    
    while running:
        for e in pygame.event.get(): 
            if e.type == pygame.QUIT:
                running = False
                break
            
            key_pressed(e)
            
        # Calcula tempo transcorrido desde
        # a última atualização 
        dt = clock.tick()
        
        
        # Atualiza posição dos objetos da tela
        update(dt, screen)
        
        # Desenha objetos na tela 
        draw_screen(screen)

        # Pygame atualiza o seu estado
        pygame.display.update() 


update_prolog()

pygame.init()
pygame.display.set_caption('INF1771 Trabalho 2 - Agente Lógico')
screen = pygame.display.set_mode((width, height+30))
load()

main_loop(screen)
pygame.quit()



