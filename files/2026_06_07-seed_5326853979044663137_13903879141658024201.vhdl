-- Seed: 5326853979044663137,13903879141658024201

library ieee;
use ieee.std_logic_1164.all;

entity bicntd is
  port (doyop : in std_logic_vector(0 downto 1); ozwv : out real; vaayl : inout real_vector(4 to 2));
end bicntd;



architecture nvh of bicntd is
  
begin
  
end nvh;

library ieee;
use ieee.std_logic_1164.all;

entity buippvywhi is
  port (mh : inout std_logic_vector(2 downto 4));
end buippvywhi;

library ieee;
use ieee.std_logic_1164.all;

architecture yqrpq of buippvywhi is
  signal gsfhrhv : real_vector(4 to 2);
  signal m : real;
  signal h : real_vector(4 to 2);
  signal qtx : real;
  signal zz : std_logic_vector(0 downto 1);
  signal tfsqnfjbwf : real_vector(4 to 2);
  signal ck : real;
  signal vcch : std_logic_vector(0 downto 1);
begin
  em : entity work.bicntd
    port map (doyop => vcch, ozwv => ck, vaayl => tfsqnfjbwf);
  trtnygqflm : entity work.bicntd
    port map (doyop => zz, ozwv => qtx, vaayl => h);
  tizlgzegie : entity work.bicntd
    port map (doyop => mh, ozwv => m, vaayl => gsfhrhv);
end yqrpq;

library ieee;
use ieee.std_logic_1164.all;

entity gtlrmk is
  port (zeevqvbb : in real; psmkkavuw : inout std_logic; ttbodfjwn : inout std_logic; zeu : buffer std_logic_vector(1 downto 0));
end gtlrmk;

library ieee;
use ieee.std_logic_1164.all;

architecture llfh of gtlrmk is
  signal jxgpuxn : real_vector(4 to 2);
  signal wu : real;
  signal v : std_logic_vector(0 downto 1);
begin
  jmwfuz : entity work.buippvywhi
    port map (mh => v);
  zemhsgulbu : entity work.bicntd
    port map (doyop => v, ozwv => wu, vaayl => jxgpuxn);
end llfh;



-- Seed after: 14905254647107379753,13903879141658024201
