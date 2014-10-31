#!/usr/bin/ruby

# Discretisation size.
$disc = 4

# Number of states.
# n > 1
$n = 3

# Trace length.
# l > 0
$l = 3

# Construct the graph as an "adjacency matrix". Each weight has an initial value of 0.
$graph = Array.new( $n) { Array.new( $n, 0)}

# Assign weights to the edges of the graph.
# The sum of the weights should add up to $disc.
$graph[0][2] = 4
$graph[1][1] = 2
$graph[1][2] = 2
$graph[2][1] = 1
$graph[2][2] = 3

# Print the secret.
puts "secret:"
for i in 0..($n-1)
	for j in 1..($n-1)
		puts "	int p#{i}#{j} = #{$graph[i][j]};"
	end
end
puts ""

# Print the belief.
puts "belief:"
for i in 0..($n-1)
	for j in 1..($n-1)
		puts "	int p#{i}#{j} = uniform 0 #{$disc};"
	end
end
puts ""

# Print the policy.
puts "policy:"
print "	max_prob_output ["
for i in 0..($n-1)
	for j in 1..($n-1)
		print " p#{i}#{j}"
	end
end
puts "] 1/1"
puts ""

# Print the regine query.
print "querydef refine ->"
for i in 1..($l)
	print " state#{i}"
end
puts " :"

# Initialize variables.
puts "	int state0 = 0;"
for i in 1..($l)
	puts "	int state#{i};"
end
puts ""

def tabs( n)
	(1..n).each {|i| print "	"}
end


def printPIF( s0, t, i, j, n)
	if j <= $disc then
		right = n - j
		left = j

		tabs( 1 + i)
		puts "if p#{s0}#{i} == #{j} then"

		if right == 0 then
			tabs( 2 + i)
			puts "state#{t} = #{i}"
		else
			tabs( 1 + i)
			puts "pif #{left} : #{right} then"
			tabs( 2 + i)
			puts "state#{t} = #{i}"
			tabs( 1 + i)
			puts "else"
			
			if i == $n - 1 then
				tabs( 2 + i)
				puts "state#{t} = 0"
			else
				printPIF( s0, t, i + 1, 0, right)
			end
		
			tabs( 1 + i)
			puts "endpif "
		end

		#puts "(* #{n} - #{j}  = #{n - j} *)"
		#puts "(* #{right} - #{j}  = #{right - j} *)"

		# Check if ended, ie there are no more probabilities left
		if n - j > 0 then
			if j != $disc then
				tabs( 1 + i)
				puts "else"
			end

			printPIF( s0, t, i, j + 1, n)
		else
			tabs( 1 + i)
		end

		#if j == $disc then
		#	tabs( 1 + i)
		#end
		print "endif"

		if j == 0 then
			puts ";"
		elsif
			print " "
		end
	end
end

def printStep( i, j)
	puts "if state#{i - 1} == #{j} then"
	printPIF( j, i, 1, 0, $disc)
	puts ""#puts ";"
	if j < ($n - 1) then
		print "	else "
		printStep( i, j + 1)
		print " "
	elsif
		print "	"
	end

	print "endif"
end

# Sample for each step.
for i in 1..($n)
	puts "	(* Step #{i}. *)"
	print "	"
	printStep( i, 0)
	
	puts ";"
	puts ""
end

# Call the query.
puts "query refine : skip"


if false then

$var = ARGV[0]
$lLim = ARGV[1].to_i
$uLim = ARGV[2].to_i
$stmt1 = ARGV[3]
$stmt2 = ARGV[4]

for i in $lLim..$uLim
	puts "		else if #{$var} == #{i} then"
	puts "			pif #{i} : #{$uLim - i} then"
	puts "				#{$stmt1}"
	puts "			else"
	puts "				#{$stmt2}"
	puts "			endpif"
end

print "		"
for i in $lLim..$uLim
	print "endif "
end

puts ";"

end
