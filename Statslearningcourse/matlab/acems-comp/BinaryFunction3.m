%% dictionary
function out = BinaryFunction3(x)
    global  call_to_oracle  x_dictionary x_out
    
    call_to_oracle = call_to_oracle +1; 
    n = size(x_dictionary,1);
    D = zeros(n,1);
    for i=1:n
        D(i) = sum(abs(x - x_dictionary(i,:)));
    end
    [M,I]  = min(D);
    out = x_out(I(1));
end