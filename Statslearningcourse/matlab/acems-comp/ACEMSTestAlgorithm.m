%% The test algorithm
function ACEMSTestAlgorithm
    %rng(1234);
    global call_to_oracle m n xorg nodes connections x_dictionary x_out binary_function
    
    arr = cell(3,0);
    arr{1} = @GetXorProblemParameters;
    arr{2} = @GetNetworkProblemParameters;
    arr{3} = @GetDictionaryProblemParameters;
    
    N = 100; % number of tests 
    worst = zeros(3,N);
    final = zeros(3,2);
    for i=1:size(arr,2)
        
        % load function and parameters
        arr{i}();
        
        % user defined methods
        learning_algorithm = @ExampleLearning;
        test_algorithm = @ExampleTest;
        % end of user defined methods

        % learning phase
        call_to_oracle = 0;
        learned_obj = learning_algorithm(n,binary_function);
        fprintf('# of calls during the learning phase - %d \n',call_to_oracle);
        % end of learning phase

        % test phase               
        id = 0;
        jump = floor(n/11);
        worst_test_perf = N;
               
        for k=1:10                     
            correct = zeros(1,N);
            cnt_true_counter = 0;            
            id = id+jump;
            for j=1:N
                test_string = zeros(1,n);
                rperm = randperm(n);
                test_string(rperm(1:id))=1;
                
                true_value = binary_function(test_string);
                user_value = test_algorithm(learned_obj,test_string);
                if(true_value == user_value)
                    cnt_true_counter = cnt_true_counter+1;
                    correct(j)=1;
                end
            end
            if(worst_test_perf >= cnt_true_counter)
                worst(i,:) = correct;
                worst_test_perf = cnt_true_counter;
                final(i,1) = id;
                final(i,2) = cnt_true_counter;
            end
            fprintf('# of correct answers for (id=%d) - %d \n',id,cnt_true_counter);
        end     
    end
    % end of test phase
    plot(1:N,cumsum(worst(1,:)),1:N,cumsum(worst(2,:)),1:N, cumsum(worst(3,:)));
    axis([0 N 0 N]);
    legend('xor','network','dictionary','Location','northwest');
    xlabel('experiment');
    ylabel('correct prediction');
    
    % print final results
    fprintf('--------------------------\n');
    fprintf('xor problem: id=%d, correct = %d \n',final(1,1),final(1,2));
    fprintf('network problem: id=%d, correct = %d \n',final(2,1),final(2,2));
    fprintf('dictionary problem: id=%d, correct = %d \n',final(3,1),final(3,2));
    fprintf('--------------------------\n');
end

function GetDictionaryProblemParameters()
     global call_to_oracle m n xorg nodes connections x_dictionary x_out binary_function
     n = 64;
     x_dictionary = rand(10,n)<=0.5;
     x_out = rand(1,10)<=0.5;
     binary_function = @BinaryFunction3;
end


function GetNetworkProblemParameters()
    global call_to_oracle m n xorg nodes connections x_dictionary x_out binary_function
    n = 128;
    m = log2(n);
    netw = load('network1.mat');
    nodes = netw.network{1};
    connections = netw.network{2};
    binary_function = @BinaryFunction2;
end

function GetXorProblemParameters()
   global call_to_oracle m n xorg nodes connections x_dictionary x_out binary_function
   n = 100; % binary string length
   m = 50;  % for XOR binary function 
   xorg = repmat([zeros(1,10), ones(1,10)],1,5);
   binary_function = @BinaryFunction1;   
end