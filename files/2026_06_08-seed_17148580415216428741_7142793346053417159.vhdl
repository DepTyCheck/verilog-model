-- Seed: 17148580415216428741,7142793346053417159



entity aeeu is
  port (zcjrotnf : in character; jtnkjxsij : out real_vector(1 to 3));
end aeeu;



architecture inwhvvw of aeeu is
  
begin
  
end inwhvvw;



entity qhnhcg is
  port (linwlk : in character; jmvy : in time);
end qhnhcg;



architecture l of qhnhcg is
  signal enqtuw : real_vector(1 to 3);
  signal d : character;
begin
  zxxptz : entity work.aeeu
    port map (zcjrotnf => d, jtnkjxsij => enqtuw);
end l;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (hf : buffer std_logic_vector(2 downto 2); ydoztautjz : linkage integer);
end y;



architecture jgd of y is
  signal pezzydsx : time;
  signal anto : character;
  signal kjyv : real_vector(1 to 3);
  signal ygeealm : character;
begin
  cxbw : entity work.aeeu
    port map (zcjrotnf => ygeealm, jtnkjxsij => kjyv);
  v : entity work.qhnhcg
    port map (linwlk => anto, jmvy => pezzydsx);
end jgd;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (ekplootw : linkage time_vector(3 downto 4); dgsdn : out std_logic; mvvdyfq : in boolean; rbjsa : in std_logic_vector(0 downto 3));
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture qnunbbnf of n is
  signal bxxhctqpxt : integer;
  signal putvhl : std_logic_vector(2 downto 2);
  signal qsscwzlfz : time;
  signal v : character;
begin
  liedqgdlj : entity work.qhnhcg
    port map (linwlk => v, jmvy => qsscwzlfz);
  mnpnzydy : entity work.y
    port map (hf => putvhl, ydoztautjz => bxxhctqpxt);
end qnunbbnf;



-- Seed after: 5623548224652384029,7142793346053417159
