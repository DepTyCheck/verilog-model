-- Seed: 14131333574605038853,7142793346053417159



entity kgavba is
  port (fqdrmkn : buffer time_vector(2 to 1); lqb : buffer real_vector(4 to 3));
end kgavba;



architecture j of kgavba is
  
begin
  
end j;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (fxrptckmjj : in time; n : in std_logic_vector(1 to 0); gqcyniabta : buffer time; wqanz : inout integer);
end w;



architecture ivk of w is
  signal ryj : real_vector(4 to 3);
  signal fywvi : time_vector(2 to 1);
  signal flb : real_vector(4 to 3);
  signal opwwzej : time_vector(2 to 1);
  signal xrcom : real_vector(4 to 3);
  signal kv : time_vector(2 to 1);
  signal jivn : real_vector(4 to 3);
  signal kmfgqgu : time_vector(2 to 1);
begin
  rgg : entity work.kgavba
    port map (fqdrmkn => kmfgqgu, lqb => jivn);
  nskcamq : entity work.kgavba
    port map (fqdrmkn => kv, lqb => xrcom);
  relsapbku : entity work.kgavba
    port map (fqdrmkn => opwwzej, lqb => flb);
  ubggf : entity work.kgavba
    port map (fqdrmkn => fywvi, lqb => ryj);
end ivk;



-- Seed after: 2058970804386019784,7142793346053417159
