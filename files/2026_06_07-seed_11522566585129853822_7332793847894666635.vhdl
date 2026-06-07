-- Seed: 11522566585129853822,7332793847894666635

library ieee;
use ieee.std_logic_1164.all;

entity hwvrdvrqr is
  port (bnfet : out std_logic_vector(0 downto 1); n : buffer real_vector(2 to 1));
end hwvrdvrqr;



architecture sicli of hwvrdvrqr is
  
begin
  
end sicli;

library ieee;
use ieee.std_logic_1164.all;

entity elcy is
  port (rtlsx : out std_logic_vector(2 downto 1); ju : linkage real);
end elcy;

library ieee;
use ieee.std_logic_1164.all;

architecture qjj of elcy is
  signal wcpiiv : real_vector(2 to 1);
  signal kvse : real_vector(2 to 1);
  signal pnbdqi : real_vector(2 to 1);
  signal tgo : std_logic_vector(0 downto 1);
begin
  moej : entity work.hwvrdvrqr
    port map (bnfet => tgo, n => pnbdqi);
  pcnqjx : entity work.hwvrdvrqr
    port map (bnfet => tgo, n => kvse);
  rdldy : entity work.hwvrdvrqr
    port map (bnfet => tgo, n => wcpiiv);
end qjj;

library ieee;
use ieee.std_logic_1164.all;

entity figrlfw is
  port (wapcz : linkage std_logic; ycz : inout time);
end figrlfw;



architecture sfjwxbwbl of figrlfw is
  
begin
  
end sfjwxbwbl;



entity gd is
  port (ktppbwx : in real; xia : buffer bit; d : in real; mugcney : in time);
end gd;

library ieee;
use ieee.std_logic_1164.all;

architecture dp of gd is
  signal kwxduqwou : std_logic_vector(2 downto 1);
begin
  ssyss : entity work.elcy
    port map (rtlsx => kwxduqwou, ju => d);
  bqjdp : entity work.elcy
    port map (rtlsx => kwxduqwou, ju => d);
end dp;



-- Seed after: 147170586638022426,7332793847894666635
