#  File: term_project.py
#  Description: Software project that allows users to explore the World Happiness Index dataset
#  Student Name: Lucas Chiang
#  Student UT EID: lmc4866
#  Partner Name: Travis Welsh
#  Partner UT EID: taw3238
#  Course Name: CS 313E
#  Unique Number: 50775
#  Date Created: April 15th, 2024
#  Date Last Modified: April 15th, 2024
"""final project"""
import math
import csv
import pandas as pd
from heaps import min_heap, max_heap


class Stats():
    """The class the has all the methods"""
    def __init__(self):
        """Loads the data set into a hash table"""

        self.hash_table = {}
        self.variables = ["Overall Rank", "Happiness Score", "GDP Per Capita", "Social Support",
                          "Healthy Life Expectancy", "Freedom to Make Life Choices", "Generosity",
                          "Perceptions of Corruption"]
        with open("2019.csv", "r") as data:
            csvreader = csv.reader(data)
            next(csvreader)
            for line in csvreader:
                self.hash_table[line[1]] = [int(line[0])] + list(map(float, line[2:]))

    def get_hash_table(self):
        """Displays the hash table"""
        return self.hash_table

    def main_menu(self):
        """Starts the main menu and controls the overall program based on user input"""

        valid_choices = ["0", "1", "2", "3", "4", "5", "6", "7", "Q"]

        print("#########################################################")
        print("Welcome to World Happiness Data Explorer Project")
        print("#########################################################")
        print("Options: (Type in the Corresponding Number to Choose an Option)")
        print("0: Display All Data")
        print("1. Search For a Country")
        print("2. Get Mean for a Category")
        print("3. Get Median for a Category")
        print("4. Get Standard Deviation for a Category")
        print("5. Get Top K countries in a Category")
        print("6. Get Bottom K countries in a Category")
        print("7. Display Category Information")
        print("Q. QUIT")
        print("#########################################################")

        while True:
            user_input = input()

            if user_input not in valid_choices:
                print("###########################")
                print("Invalid Choice!, Try Again!")
                print("###########################")
                continue

            if user_input == "0":
                print(self.display_all())
                self.main_menu()
                return

            if user_input == "1":
                print("################################################")
                print("Type in the Country You Want to Look Up Data For")
                print("################################################")
                while True:
                    user_input = input().title()
                    if user_input not in self.hash_table:
                        print("###########################")
                        print("Country not found! Try again!")
                        print("###########################")
                        print()
                        continue
                    self.search(user_input)
                    break
                self.main_menu()
                return

            if user_input == "2":
                mean_choices = ["0", "1", "2", "3", "4", "5", "6", "7", "M"]
                print("#########################################################")
                print("Choose a Category to Find Mean (Type Corresponding Number)")
                print("0. Happiness Score")
                print("1. GDP Per Capita")
                print("2. Social Support")
                print("3. Healthy Life Expectancy")
                print("4. Freedom to Make Life Choices")
                print("5. Generosity")
                print("6. Perceptions of Corruption")
                print("M. Main Menu")
                print("#########################################################")
                user_input = input()
                while True:
                    if user_input not in mean_choices:
                        continue
                    if user_input == "M":
                        self.main_menu()
                    print()
                    print("Mean Value for Category:", self.get_mean(int(user_input) + 1))
                    print()
                    self.main_menu()
                    return

            if user_input == "3":
                median_choices = ["0", "1", "2", "3", "4", "5", "6", "7", "M"]
                print("#########################################################")
                print("Choose a Category to Find The Median (Type Corresponding Number)")
                print("0. Happiness Score")
                print("1. GDP Per Capita")
                print("2. Social Support")
                print("3. Healthy Life Expectancy")
                print("4. Freedom to Make Life Choices")
                print("5. Generosity")
                print("6. Perceptions of Corruption")
                print("M. Main Menu")
                print("#########################################################")
                user_input = input()
                while True:
                    if user_input not in median_choices:
                        print("###########################")
                        print("Invalid Choice!, Try Again!")
                        print("###########################")
                        print()
                        continue
                    if user_input == "M":
                        self.main_menu()
                        return
                    print("Median Value for Category:", self.get_median(int(user_input) + 1))
                    self.main_menu()
                    return

            if user_input == "4":
                stdev_choices = ["0", "1", "2", "3", "4", "5", "6", "7", "M"]
                print("#########################################################")
                print("Choose a Category to Find Standard Deviation (Type Corresponding Number)")
                print("0. Happiness Score")
                print("1. GDP Per Capita")
                print("2. Social Support")
                print("3. Healthy Life Expectancy")
                print("4. Freedom to Make Life Choices")
                print("5. Generosity")
                print("6. Perceptions of Corruption")
                print("M. Main Menu")
                print("#########################################################")
                user_input = input()
                while True:
                    if user_input not in stdev_choices:
                        print("Invalid Choice!, Try Again!")
                        continue
                    if user_input == "M":
                        self.main_menu()
                        return
                    print("Standard Deviation for the Category "
                          "is", self.get_std_dev(int(user_input) + 1))
                    self.main_menu()
                    return

            if user_input == "5":
                topk_choices = ["0", "1", "2", "3", "4", "5", "6", "7", "M"]
                print("#########################################################")
                print("Choose a Category to Find Top K Countries (Type Corresponding Number)")
                print("0. Happiness Score")
                print("1. GDP Per Capita")
                print("2. Social Support")
                print("3. Healthy Life Expectancy")
                print("4. Freedom to Make Life Choices")
                print("5. Generosity")
                print("6. Perceptions of Corruption")
                print("M. Main Menu")
                print("##################################"
                      "#######################")
                while True:
                    input_one = input()
                    if input_one not in topk_choices:
                        print("Invalid Choice! Try Again!")
                        continue
                    if user_input == "M":
                        self.main_menu()
                        return
                    break

                while True:
                    print("#########################################################")
                    print("Enter A Number Between 1-156")
                    print("#########################################################")
                    try:
                        input_two = int(input().strip())
                    except ValueError:
                        print("###################################")
                        print("Not a Number! Try Again!")
                        print("###################################")
                        print()
                        continue

                    if input_two < 1 or input_two > 156:
                        print("####################################")
                        print("Number Not Within Bounds. Try Again!")
                        print("####################################")
                        continue
                    print()
                    print(f"The Top {input_two} Countries For The "
                          f"{self.variables[int(input_one) + 1]} Category Are:")
                    print()
                    self.get_top_k(input_two, int(input_one) + 1)
                    print()
                    self.main_menu()
                    return

            if user_input == "6":
                bottomk_choices = ["0", "1", "2", "3", "4", "5", "6", "7", "M"]
                print("#########################################################")
                print("Choose a Category to Find Bottom K Countries (Type Corresponding Number)")
                print("0. Happiness Score")
                print("1. GDP Per Capita")
                print("2. Social Support")
                print("3. Healthy Life Expectancy")
                print("4. Freedom to Make Life Choices")
                print("5. Generosity")
                print("6. Perceptions of Corruption")
                print("M. Main Menu")
                print("#########################################################")
                while True:
                    input_one = input()
                    if input_one not in bottomk_choices:
                        print("Invalid Choice! Try Again!")
                        continue
                    if user_input == "M":
                        self.main_menu()
                        return
                    break

                while True:
                    print("#########################################################")
                    print("Enter A Number Between 1-156")
                    print("#########################################################")
                    try:
                        input_two = int(input().strip())
                    except ValueError:
                        print("###################################")
                        print("Not a Number! Try Again!")
                        print("###################################")
                        print()
                        continue
                    if input_two < 1 or input_two > 156:
                        print("####################################")
                        print("Number Not Within Bounds. Try Again!")
                        print("####################################")
                        print()
                        continue
                    print()
                    print(
                        f"The Bottom {input_two} Countries For The "
                        f"{self.variables[int(input_one) + 1]} Category Are:")
                    print()
                    self.get_bottom_k(input_two, int(input_one) + 1)
                    print()
                    self.main_menu()
                    return

            if user_input == "7":
                self.display_cat_info()
                self.main_menu()
                return

            if user_input == "Q":
                return

    def display_all(self):
        """displays the dataset"""
        pd.options.display.max_rows = 1000
        pd.set_option('expand_frame_repr', False)
        file = "2019.csv"
        df = pd.read_csv(file)
        pd.options.display.max_columns = len(df.columns)
        return df

    def display_cat_info(self):
        """displays info about each category"""
        print()
        print("Context of the Dataset: The happiness scores and "
              "rankings use data from the Gallup World Poll.\n"
              "The scores are based on answers to the main life evaluation"
              " question asked in the poll.\nThis question, "
              "known as the Cantril ladder, asks respondents to think "
              "of a ladder with the best possible\nlife for "
              "them being a 10 and the worst possible life being a 0 "
              "and to rate their own current lives\non that scale"
              ". The columns following the happiness score estimate"
              " the extent to which each of six\nfactors, economic "
              "production, social support, life expectancy, "
              "freedom, absence of corruption, and generosity\ncontribute"
              " to making life evaluations higher in each country than they are in Dystopia.\n")
        print("Happiness Score: The overall score on a scale of 0 to 10.")
        print("GDP Per Capita: the extent to which GDP per capita contributes in "
              "evaluating the happiness in each country")
        print("Social Support: the extent to which social support contributes in "
              "evaluating the happiness in each country")
        print("Healthy Life Expectancy: the extent to which healthy life expectancy contributed in "
              "evaluating the happiness in each country")
        print("Freedom to Make Life Choices: the extent to "
              "which freedom to make life choices contributed in "
              "evaluating the happiness in each country")
        print("Generosity: the extent to which generosity contributed in "
              "evaluating the happiness in each country")
        print("Perceptions of Corruption: the extent "
              "to which perceptions of corruption contributed in "
              "evaluating the happiness in each country")
        print()

    def get_mean(self, index):
        """Returns the mean of a category"""
        vals = [list_of_values[index] for list_of_values in self.hash_table.values()]
        return round(sum(vals) / len(self.hash_table), 3)

    def search(self, country):
        """Returns data for the country requested"""
        n = 0
        print()
        print(f"Data for {country}\n")
        for i in self.hash_table[country]:
            print(f"{self.variables[n]}: {i}")
            n = n + 1
        print()
        return self.hash_table[country]

    def get_median(self, index):
        """Returns the median of a category"""
        vals = [list_of_values[index] for list_of_values in self.hash_table.values()]
        self.merge_sort(vals, 0, len(vals) - 1)
        if len(vals) % 2 == 1:
            return vals[int(len(vals) / 2 - 1)]
        return round((vals[int(len(vals) / 2 - 1)] + vals[int(len(vals) / 2)]) / 2, 3)

    def get_std_dev(self, index):
        """Returns the standard deviation of a category"""
        vals = [list_of_values[index] for list_of_values in self.hash_table.values()]
        mean = self.get_mean(index)
        return round(math.sqrt(sum([(mean - x) ** 2 for x in vals]) / len(self.hash_table)), 3)

    def get_top_k(self, amount, index):
        """Returns the Top K countries of the indicated category"""
        check_list = []
        lst = self.create_pairs(index)
        heap = max_heap()
        heap.build_max_heap(lst)
        for _ in range(amount):
            res = heap.extract_max()
            check_list.append(res)
            print(f"{res[0]}: {res[1]}")
        return check_list

    def get_bottom_k(self, amount, index):
        """Returns the Bottom K countries of the indicated category"""
        check_list = []
        lst = self.create_pairs(index)
        heap = min_heap()
        heap.build_min_heap(lst)
        for _ in range(amount):
            res = heap.extract_min()
            check_list.append(res)
            print(f"{res[0]}: {res[1]}")
        return check_list

    def merge_sort(self, arr, p, r):
        """Performs Merge Sort where arr is the array, p is the left index and r is the right"""
        if p < r:
            q = math.floor((p + r) / 2)
            self.merge_sort(arr, p, q)
            self.merge_sort(arr, q + 1, r)
            self.merge(arr, p, q, r)

    def merge(self, arr, p, q, r):
        """Merge procedure for merge sort"""
        left = arr[p: q + 1]
        right = arr[q + 1: r + 1]
        # adding sentinel values
        left.append(float("inf"))
        right.append(float("inf"))

        i, j = 0, 0
        for k in range(p, r + 1):
            if left[i] <= right[j]:
                arr[k] = left[i]
                i += 1
            else:
                arr[k] = right[j]
                j += 1

    def create_pairs(self, index):
        """Creates list to use for heaps"""
        pair_list = []
        for i in self.hash_table:
            pair_list.append((i, self.hash_table[i][index]))
        return pair_list


if __name__ == "__main__":
    s = Stats()
    s.main_menu()
