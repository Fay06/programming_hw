# We will be implimenting a simple database table using Ruby data structures to store the data.
# The class Tuple represents an entry in a table.
# The class Table represents a collection of tuples.

class Tuple
    @@nums = {}
    @@nums.default = 0

    # data is an array of values for the tuple
    def initialize(data)
        @Tuple = Array.new
        for i in data
            @Tuple.push(i)
        end
        a = @Tuple.length()
        @@nums[a] += 1
    end

    # This method returns the number of entries in this tuple
    def getSize()
        @Tuple.length()
    end

    # This method returns the data at a particular index of a tuple (0 indexing)
    # If the provided index exceeds the largest index in the tuple, nil should be returned.
    # index is an Integer representing a valid index in the tuple.
    def getData(index)
        @Tuple.at(index)
    end

    # This method should return the number of tuples of size n that have ever been created
    # hint: you should use a static variable
    # hint2: a hash can be helpful (though not strictly necessary!)
    def self.getNumTuples(n)
        @@nums[n]
    end
end

class Table
    # column_names is an Array of Strings
    def initialize(column_names)
        @Table = column_names
        @Tuples = Array.new
    end

    # This method inserts a tuple into the table.
    # Note that tuples inserted into the table must have the right number of entries
    # I.e., the tuple should be the size of column_names
    # If the tuple is the correct size, insert it and return true
    # otherwise, DO NOT insert the tuple and return false instead.
    # tuple is an instance of class Tuple declared above.
    def insertTuple(tuple)
        if tuple.getSize == @Table.length()
            @Tuples.push(tuple)
            true
        else
            false
        end
    end
    
    # This method returns the number of tuples in the table
    def getSize
        @Tuples.length()
    end

    # This method selects columns from the table, equivalent to a SQL `select column_names from table` query.
    # This should return a new table that is identical in structure to this one,
    # except only including the columns listed in the `column_names` array
    # EXAMPLE:
    #  column_1 | column_2 | column_3 | column_4
    # -------------------------------------------
    #     1     |    2     |     3    |     4    
    #     2     |    3     |     4    |     1    
    #     3     |    4     |     1    |     2      
    #     4     |    1     |     2    |     3    
    # Note that this is a table made up of 4-element tuples
    # selectTuples(["column_1", "column_3"]) should return a table with the structure
    #  column_1 | column_3
    # ---------------------
    #     1     |     3    
    #     2     |     4    
    #     3     |     1      
    #     4     |     2    
    # Notice that we NOW have a table of 2-element tuples
    # hint: to find the index of an element in an array, you can use arr.index(element)
    def selectTuples(column_names)
        array = Table.new(column_names)
        for i in @Tuples
            data = Array.new()
            for j in column_names
                a = @Table.index(j)
                data.push(i.getData(a))
            end
            tuple = Tuple.new(data)
            array.insertTuple(tuple)
        end
        array
    end

    # This should return an array of the tuples present in the table
    def getTuples()
        array = Array.new()
        for i in @Tuples
            array.push(i)
        end
        array
    end
end
