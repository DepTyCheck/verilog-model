-- Seed: 3664449243027390712,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity mdktlbyfqp is
  port (iayxonwoz : in std_logic_vector(4 downto 4); orimmq : buffer time; dqnwkxwr : in real; ljqlusls : linkage boolean);
end mdktlbyfqp;

architecture bru of mdktlbyfqp is
  
begin
  -- Single-driven assignments
  orimmq <= 1 hr;
end bru;

library ieee;
use ieee.std_logic_1164.all;

entity gki is
  port (ezuvdy : in std_logic_vector(3 to 1); m : out std_logic; tfpogqq : linkage time; llu : linkage time);
end gki;

library ieee;
use ieee.std_logic_1164.all;

architecture b of gki is
  signal xkigan : boolean;
  signal gmtj : real;
  signal pbcqdzs : time;
  signal exroz : std_logic_vector(4 downto 4);
  signal hybinqv : boolean;
  signal dlmjiat : time;
  signal lmm : std_logic_vector(4 downto 4);
  signal wmiccotr : boolean;
  signal rrun : real;
  signal clyhtga : time;
  signal wx : std_logic_vector(4 downto 4);
begin
  qelhi : entity work.mdktlbyfqp
    port map (iayxonwoz => wx, orimmq => clyhtga, dqnwkxwr => rrun, ljqlusls => wmiccotr);
  f : entity work.mdktlbyfqp
    port map (iayxonwoz => lmm, orimmq => dlmjiat, dqnwkxwr => rrun, ljqlusls => hybinqv);
  uoagfdr : entity work.mdktlbyfqp
    port map (iayxonwoz => exroz, orimmq => pbcqdzs, dqnwkxwr => gmtj, ljqlusls => xkigan);
end b;

library ieee;
use ieee.std_logic_1164.all;

entity wxzi is
  port (vjbom : out std_logic_vector(4 to 2); indy : out real; bbyd : in time);
end wxzi;

library ieee;
use ieee.std_logic_1164.all;

architecture mvmxmli of wxzi is
  signal dfymfv : boolean;
  signal sfzp : real;
  signal iu : time;
  signal fvfqhwoowu : std_logic_vector(4 downto 4);
  signal lfjpg : time;
  signal dwomvrzxf : time;
  signal rqk : std_logic;
begin
  vzp : entity work.gki
    port map (ezuvdy => vjbom, m => rqk, tfpogqq => dwomvrzxf, llu => lfjpg);
  szh : entity work.mdktlbyfqp
    port map (iayxonwoz => fvfqhwoowu, orimmq => iu, dqnwkxwr => sfzp, ljqlusls => dfymfv);
  
  -- Single-driven assignments
  indy <= 8#6_5_1.1736#;
  sfzp <= 2_1_4.3;
  
  -- Multi-driven assignments
  fvfqhwoowu <= fvfqhwoowu;
  rqk <= 'W';
end mvmxmli;



-- Seed after: 2988921676306998602,13788294582928374589
