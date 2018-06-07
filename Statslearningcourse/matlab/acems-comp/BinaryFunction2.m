%% network
function out = BinaryFunction2(x)
    global  nodes connections   call_to_oracle m n
    
    call_to_oracle = call_to_oracle +1; 
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
    out = x;
end