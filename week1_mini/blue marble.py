import os
import os.path as path
import random
PATH='./play/'
if not path.exists(PATH):
    os.makedirs(PATH)

def start():
    print('게임을 시작합니다.')

def showPlayer():       #현재 참여한 플레이어 보여주기
    print('현재 참여한 플레이어')
    for addr in os.listdir(PATH):
        print(addr[:-4])

def player(name):     #플레이어명 txt파일 만들기
    filename=name+'.txt'
    print(f'{name}님 게임에 참여하신 것을 환영합니다.^^')
    with open(PATH+filename,'w',encoding='utf-8') as f:
        f.write(f'{0}')

def dice(name):         #주사위 돌리기
    filename=name+'.txt'
    a1=random.randint(1,6)
    a2=random.randint(1,6)
    a=a1+a2
    print(f'첫번째 주사위 {a1}\n두번째 주사위 {a2}\n주사위 합 {a}')
    with open(PATH + filename, 'r', encoding='utf-8') as f:
        r=f.read()
        r=int(r)
        t=a+r
    with open(PATH+filename,'w',encoding='utf-8') as f:
        f.write(f'{t}')
    print(f'{name}님 현재 {t}칸 입니다.' )

#특별한 칸 5의 배수
def spe(name):
    filename = name + '.txt'
    with open(PATH + filename, 'r', encoding='utf-8') as f:
        r=f.read()
        r=int(r)
    c = random.randint(1, 10)
    for i in range(1,10):
        if r==7*i:
            if c<5:
                print(f'{name}은 앞으로 5칸 이동')
                r+=5
                with open(PATH + filename, 'w', encoding='utf-8') as f:
                    f.write(f'{r}')
            else:
                print(f'{name}은 뒤로 3칸 이동')
                r-=3
                with open(PATH + filename, 'w', encoding='utf-8') as f:
                    f.write(f'{r}')
