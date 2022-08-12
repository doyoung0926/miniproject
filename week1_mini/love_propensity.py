#----------------------------------------------------------------------------------------
# 연애성향 테스트 (적극적/소극적)
#----------------------------------------------------------------------------------------
PATH=r'C:/Users/User/Desktop/uuuu/'       # 저장된 파일 경로
#----------------------------------------------------------------------------------------
# 기    능 : 연애성향 테스트 점수 도출
# 함 수 명 : Loving_Tendencies_Score
# 파라미터 : 없음
# 반 환 값 : total
#----------------------------------------------------------------------------------------
def Loving_Tendencies_Score():
    total = 0               # 초기 점수
    for i in range(1,4):
        while True:
            with open(PATH + '연애성향' + str(i) + '.txt', 'r', encoding='utf-8') as f:
                print(f.read())
                a = int(input("정답을 입력해 주세요: "))
                if a == 1:
                    total += 0
                    break      # while문을 빠져 나간다.
                elif a == 2:
                    total += 1
                    break
                else:
                    print('잘못된 번호입니다. 다시 입력하세요.')
    return total
#----------------------------------------------------------------------------------------
# 기    능 : 점수에 대한 해당 값으로 결과를 도출
# 함 수 명 : Loving_Tendencies_finish
# 파라미터 : total
# 반 환 값 : 없음
#----------------------------------------------------------------------------------------
def Loving_Tendencies_finish(total):    # 점수를 매개변수로 넣는다.
    if 0<=total<=1:                     # 점수에 해당 구간을 찾는다.
        with open(PATH + 'passive.txt', 'r', encoding='utf-8') as f:
            x=f.read()                      # 경로에 있는 텍스트 파일을 읽고 변수 x에 저장한다.
            with open(PATH + 'all_result.txt','w',encoding='utf-8') as fi:
                fi.write(x)                 # 최종 파일에 읽어드린 x 내용을 입력한다.
    elif 2<=total<=3:
        with open(PATH + 'active.txt', 'r', encoding='utf-8') as f:
            x = f.read()
            with open(PATH + 'all_result.txt', 'w', encoding='utf-8') as fi:
                fi.write(x)
#----------------------------------------------------------------------------------------

# s=Loving_Tendencies_Score()
# Loving_Tendencies_finish(s)