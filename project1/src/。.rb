require_relative "graph.rb"

class Synsets
    def initialize
        @hash = {}
    end

    def load(synsets_file)
        invalid = Array.new
        line_num = 0
        temp = {}
        File.foreach(synsets_file) do |line|
            line_num += 1
            # check nouns
            if line =~ /^id:\s(\d+)\ssynset:\s([\w\_\-\.\'\/]+(,[\w\_\-\.\'\/]+)*)$/
                id = $1.to_i
                noun = $2
                nouns = noun.split(",")
                #check id and noun
                if id < 0 or nouns.empty?
                    invalid.push(line_num)
                elsif temp[id] or @hash[id]
                    invalid.push(line_num)
                else
                    temp[id] = nouns
                end
            else
                invalid.push(line_num)
            end
        end
        #output
        if invalid.empty?
            temp.each do |k,v|
                addSet(k,v)
            end
            nil
        else
            invalid
        end
    end

    def addSet(synset_id, nouns)
        if synset_id < 0 or nouns.empty?
            false
        elsif @hash[synset_id]
            false
        else
            @hash[synset_id] = nouns
            true
        end
    end

    def lookup(synset_id)
        if !@hash[synset_id]
            []
        else
            @hash[synset_id]
        end
    end

    def findSynsets(to_find)
        if to_find.class == String
            data = Array.new
            @hash.each do |k, v|
                v.each do |value|
                    if value == to_find
                        data.push(k)
                    end
                end
            end
            data
        elsif to_find.class == Array
            data = {}
            for i in to_find
                arr = Array.new
                @hash.each do |k, v|
                    v.each do |value|
                        if value == i
                            arr.push(k)
                        end
                    end
                end
                data[i] = arr
            end
            data
        else
            nil
        end
    end
end

class Hypernyms
    def initialize
        @graph = Graph.new
    end

    def load(hypernyms_file)
        invalid = Array.new
        line_num = 0
        temp = {}
        File.foreach(hypernyms_file) do |line|
            line_num += 1
            # check line
            if line =~ /^from:\s(\d+)\sto:\s(\d+(,\d+)*)$/
                from = $1.to_i
                to = $2
                tos = to.split(",")
                #check source and destination
                for i in tos
                    if from < 0 or i.to_i < 0 or from == to.to_i
                        invalid.push(line_num)
                    else
                        temp[from] = tos
                    end   
                end            
            else
                invalid.push(line_num)
            end
        end
        #output
        if invalid.empty?
            temp.each do |k, v|
                v.each do |i|
                    addHypernym(k, i.to_i)
                end
            end
            nil
        else
            invalid
        end
    end

    def addHypernym(source, destination)
        if source < 0 or destination < 0 or source == destination
            false
        else
            if !@graph.hasVertex?(source)
                @graph.addVertex(source)
            end
            if !@graph.hasVertex?(destination)
                @graph.addVertex(destination)
            end
            if !@graph.hasEdge?(source, destination)
                @graph.addEdge(source, destination)
            end
            true
        end
    end

    def lca(id1, id2)
        if !@graph.hasVertex?(id1) or !@graph.hasVertex?(id2)
            nil
        else
            arr = Array.new
            h1 = @graph.bfs(id1)
            h2 = @graph.bfs(id2)
            common = find_common(h1, h2)
            min = common.values.min
            common.each do |id, length|
                if length == min
                    arr.push(id)
                end
            end
            if arr.empty?
                nil
            else
                arr
            end
        end
    end

    def find_common(hash1, hash2) 
        common_elements = {}
        hash1.each do |id,length|
            if hash2.include?(id) 
                common_elements[id] = hash1[id] + hash2[id]
            end 
        end 
        common_elements 
    end

    def load_command(hypernyms_file, synsets)
        invalid = Array.new
        line_num = 0
        temp = {}
        File.foreach(hypernyms_file) do |line|
            line_num += 1
            # check line
            if line =~ /^from:\s(\d+)\sto:\s(\d+(,\d+)*)$/
                from = $1.to_i
                to = $2
                tos = to.split(",")
                #check source and destination
                for i in tos
                    if from < 0 or i.to_i < 0 or from == to.to_i or synsets.lookup(from).empty? or synsets.lookup(i.to_i).empty?
                        invalid.push(line_num)
                    else
                        temp[from] = tos
                    end   
                end            
            else
                invalid.push(line_num)
            end
        end
        
        #output
        if invalid.empty?
            temp.each do |k, v|
                v.each do |i|
                    addHypernym(k, i.to_i)
                end
            end
            nil
        else
            invalid
        end
    end
end

class CommandParser
    def initialize
        @synsets = Synsets.new
        @hypernyms = Hypernyms.new
    end

    def parse(command)
        hash = {}
        com = command.split(" ")
        case com[0]
        when "load"
            hash[:recognized_command] = :load
            if com.length() != 3 or !com[1] =~ /[\w\_\-\.\'\/]+/ or !com[2] =~ /[\w\_\-\.\'\/]+/
                hash[:result] = :error
            elsif @synsets.load(com[1]).nil?
                if @hypernyms.load_command(com[2], @synsets).nil?
                    hash[:result] = true
                else
                    hash[:result] = false
                end
            else
                hash[:result] = false
            end
        when "lookup"
            hash[:recognized_command] = :lookup
            if com.length() != 2 or com[1].to_i < 0
                hash[:result] = :error
            else
                hash[:result] = @synsets.lookup(com[1].to_i)
            end
        when "find"
            hash[:recognized_command] = :find
            if com.length() != 2 or !com[1] =~ /[\w\_\-\.\'\/]+/
                hash[:result] = :error
            else
                hash[:result] = @synsets.findSynsets(com[1])
            end
        when "findmany"
            hash[:recognized_command] = :findmany
            if com.length() != 2 or !com[1] =~ /[\w\_\-\.\'\/]+(,[\w\_\-\.\'\/]+)*/
                hash[:result] = :error
            else
                nouns = com[1].split(",")
                hash[:result] = @synsets.findSynsets(nouns)
            end
        when "lca"
            hash[:recognized_command] = :lca
            if com.length() != 3 or id1 < 0 or id2 < 0
                hash[:result] = :error
            else
                hash[:result] = @hypernyms.lca(com[1].to_i, com[2].to_i)
            end
        else
            hash[:recognized_command] = :invalid
        end
        hash
    end

end

