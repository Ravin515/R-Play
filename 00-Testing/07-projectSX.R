cmpny<- fread('C:/Users/Mr.Stylee/Desktop/绍兴新金融发展规划/公司基本信息.csv', encoding = "UTF-8")[-1]
cp.cmpny <- cmpny[Pftn == "东莞市" | Pftn == "绍兴市"]
sx <- cmpny[Pftn == "绍兴市"]