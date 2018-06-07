%% XOR
function out = BinaryFunction1(x)
    global m xorg call_to_oracle
    
    call_to_oracle = call_to_oracle +1;
    out = (sum(xor(x,xorg)) > m);    
end