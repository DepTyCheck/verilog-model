-- Seed: 1669761021327698319,10871023049702252113

entity p is
  port (qytm : buffer character);
end p;

architecture e of p is
  
begin
  
end e;

library ieee;
use ieee.std_logic_1164.all;

entity swscvxo is
  port (zlgunntdc : inout std_logic; xbnvzwgqks : in std_logic);
end swscvxo;

architecture omr of swscvxo is
  signal di : character;
  signal djuojrwpsm : character;
begin
  crqhlt : entity work.p
    port map (qytm => djuojrwpsm);
  nbjvjzdr : entity work.p
    port map (qytm => di);
  
  -- Multi-driven assignments
  zlgunntdc <= xbnvzwgqks;
  zlgunntdc <= 'X';
  zlgunntdc <= zlgunntdc;
  zlgunntdc <= xbnvzwgqks;
end omr;



-- Seed after: 17225540133161503042,10871023049702252113
