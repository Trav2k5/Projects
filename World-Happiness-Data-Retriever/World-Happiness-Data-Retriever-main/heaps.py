# MIN HEAP Implementation
class min_heap:
    def __init__(self):
        self.heap_list = []

    def get_min_heap(self):
        return self.heap_list

    @property
    def size(self):
        return len(self.heap_list) - 1 
 
    def parent(self, index):
        return index // 2


    def l_child(self, index):
        return 2 * index

    def r_child(self, index):
        return (2 * index) + 1

    def swap(self, pos1, pos2):
        self.heap_list[pos1], self.heap_list[pos2] = self.heap_list[pos2], self.heap_list[pos1]

    def min_heapify(self, i):
        l = self.l_child(i)
        r = self.r_child(i)

        if l <= self.size and self.heap_list[l][1] < self.heap_list[i][1]:
            smallest = l
        else:
            smallest = i

        if r <= self.size and self.heap_list[r][1] < self.heap_list[smallest][1]:
            smallest = r 

        if smallest != i :
            self.swap(i, smallest)
            self.min_heapify(smallest)

    def build_min_heap(self, lst):
        lst.insert(0, ("placeholder", float("-inf")))

        self.heap_list = lst

        for i in range(len(lst) // 2 , 0, -1):
            self.min_heapify(i)

    def extract_min(self):
        minimum = self.heap_list.pop(1)
        if(self.size > 1):
            self.heap_list.insert(1, self.heap_list.pop())
            self.min_heapify(1)
        return minimum


# MAX HEAP implementation 
class max_heap:
    def __init__(self):
        self.heap_list = []
    
    def get_max_heap(self):
        return self.heap_list

    @property
    def size(self):
        return len(self.heap_list) - 1 
 
    def parent(self, index):
        return index // 2


    def l_child(self, index):
        return 2 * index

    def r_child(self, index):
        return (2 * index) + 1

    def swap(self, pos1, pos2):
        self.heap_list[pos1], self.heap_list[pos2] = self.heap_list[pos2], self.heap_list[pos1]

    def max_heapify(self, i):
        l = self.l_child(i)
        r = self.r_child(i)

        if l <= self.size and self.heap_list[l][1] > self.heap_list[i][1]:
            greatest = l
        else:
            greatest = i

        if r <= self.size and self.heap_list[r][1] > self.heap_list[greatest][1]:
            greatest = r 

        if greatest != i :
            self.swap(i, greatest)
            self.max_heapify(greatest)

    def build_max_heap(self, lst):
        lst.insert(0, ("placeholder", float("inf")))

        self.heap_list = lst

        for i in range(len(lst) // 2 , 0, -1):
            self.max_heapify(i)

    def extract_max(self):
        maximum = self.heap_list.pop(1)
        if(self.size > 1):
            self.heap_list.insert(1, self.heap_list.pop())
            self.max_heapify(1)
        return maximum