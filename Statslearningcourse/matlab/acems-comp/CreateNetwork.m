%% An example network creation
function CreateNetwork

    %rng(12345);
    n = 128;          % input size
    m = log2(n);      % # of layers
    
    nodes = cell(m+1,0);
    
    % set tresholds
    for i=1:m+1
         arr = zeros(1,2^(m-i+1));         
         if(i>1)
             for j=1:size(arr,2)
                 arr(1,j) = 1;
                 if(rand<=0.6)
                      arr(1,j) = 2;
                 end
             end
         end
         nodes{i} = arr;
    end
    
    % set edges (at least one) 
    connections = cell(m,0);
    for level=1:m        
        mylevel = nodes{level};
        nextlevel = nodes{level+1};
        l1sz = size(mylevel,2);
        l2sz = size(nextlevel,2);
        
        conn_arr = zeros(l1sz, l2sz);
        
        if(l2sz<=2)
            conn_arr = ones(l1sz, l2sz);
        else
            for j=1:l1sz
                rperm = randperm(l2sz);
                for k=1:1
                    u = rperm(k);
                    conn_arr(j,u)=1;
                end
            end                
        end
        connections{level} = conn_arr;
    end
    
    network = cell(2,0);
    network{1} = nodes;
    network{2} = connections;
    
    save('network1','network');
    
    % calculate for input (suppose that n and m are given)
    N = 1000;
    cntone = 0;
    for i=1:N
        x = rand(1,n)<=0.5;
        test = Evaluate(network,n,m,x);
        if(test==1)
           cntone = cntone+1;
        end
    end
    cntone/N
end

function res = Evaluate(network,n,m,x)
    nodes =  network{1};
    connections = network{2};
    
    for i=1:m
       target = zeros(1, 2^(m-i));
       conn_mat = connections{i};
       for j=1:size(x,2)
           if(x(j)==1)
               ind = find(conn_mat(j,:)==1);
               target(ind) = target(ind)+1;
           end
       end
       nodes_at_next_level = nodes{i+1};
       x = zeros(1,size(target,2));
       for j=1:size(target,2)
          if(target(j)>=nodes_at_next_level(j))
             x(j) = 1; 
          end
       end
    end
    res = x;
end








