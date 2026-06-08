-- Seed: 10927394766201413209,7142793346053417159

library ieee;
use ieee.std_logic_1164.all;

entity wqv is
  port (rhjkx : buffer std_logic_vector(4 downto 4); xfhxmfho : out integer; jru : inout boolean_vector(0 to 4); zx : buffer time);
end wqv;



architecture dq of wqv is
  
begin
  
end dq;



entity dzoked is
  port (wmxgqxrhn : out time);
end dzoked;

library ieee;
use ieee.std_logic_1164.all;

architecture an of dzoked is
  signal nz : time;
  signal yehjseg : boolean_vector(0 to 4);
  signal jyaynmr : integer;
  signal kbwcwswrj : std_logic_vector(4 downto 4);
begin
  vgmzz : entity work.wqv
    port map (rhjkx => kbwcwswrj, xfhxmfho => jyaynmr, jru => yehjseg, zx => nz);
end an;



entity xxwqghpoq is
  port (f : linkage integer_vector(0 downto 3); nws : out time);
end xxwqghpoq;



architecture fggcvcf of xxwqghpoq is
  signal kcbrkyq : time;
begin
  uuffphphfc : entity work.dzoked
    port map (wmxgqxrhn => kcbrkyq);
end fggcvcf;



entity nrkrfqqknw is
  port (qxgbyrtj : in real; hfilmu : out integer);
end nrkrfqqknw;

library ieee;
use ieee.std_logic_1164.all;

architecture qwf of nrkrfqqknw is
  signal rf : time;
  signal uqrqmvnr : integer_vector(0 downto 3);
  signal vyhvajmz : time;
  signal hrgfvcxmfx : boolean_vector(0 to 4);
  signal iqasdwib : integer;
  signal dmli : std_logic_vector(4 downto 4);
begin
  kkbdaosix : entity work.wqv
    port map (rhjkx => dmli, xfhxmfho => iqasdwib, jru => hrgfvcxmfx, zx => vyhvajmz);
  cb : entity work.xxwqghpoq
    port map (f => uqrqmvnr, nws => rf);
end qwf;



-- Seed after: 12099764235394660782,7142793346053417159
