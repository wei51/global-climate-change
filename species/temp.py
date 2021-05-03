def dfs(self, A,i,j,rows,cols,queue):
        
        if i<0 or i>=rows or j<0 or j>=cols or A[i][j]!=1: return
        A[i][j]=2  # Mark as island
        queue.append([i,j])
        self.dfs(A,i-1,j,rows,cols,queue)
        self.dfs(A,i+1,j,rows,cols,queue)
        self.dfs(A,i,j-1,rows,cols,queue)
        self.dfs(A,i,j+1,rows,cols,queue)
        
    def shortestBridge(self, A: List[List[int]]) -> int:
        
        rows = len(A)
        cols = len(A[0])
        dirs = [[1,0],[0,1],[-1,0],[0,-1]]
        
        queue = []
        found = False
        for i in range(rows):
            for j in range(cols):
                if A[i][j]==1 and not found:
                    self.dfs(A,i,j,rows,cols,queue)
                    found=True
        step = 0
        while queue:
            size = len(queue)
            for i in range(size):
                r,c = queue[0]
                queue = queue[1:]
                for dir in dirs:
                    x = r+dir[0]
                    y = c+dir[1]
                    
                    if x>=0 and x<rows and y>=0 and y<cols and A[x][y]!=2: #valid and not visited
                        
                        if A[x][y]==1: return step
                        if A[x][y]==0: 
                            queue.append([x,y])
                            A[x][y]=2   
            step+=1
        return -1