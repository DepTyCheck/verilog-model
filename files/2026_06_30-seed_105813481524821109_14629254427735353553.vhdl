-- Seed: 105813481524821109,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity kkwxbkh is
  port (aucjlsbjxz : out std_logic_vector(0 downto 2); dr : out real; pguj : inout real);
end kkwxbkh;

architecture vstnie of kkwxbkh is
  
begin
  -- Multi-driven assignments
  aucjlsbjxz <= (others => '0');
end vstnie;

entity jxu is
  port (uhxfvrzk : inout character);
end jxu;

library ieee;
use ieee.std_logic_1164.all;

architecture tntk of jxu is
  signal hrtetkxlmj : real;
  signal nmicjny : real;
  signal eymoie : std_logic_vector(0 downto 2);
begin
  svusikvu : entity work.kkwxbkh
    port map (aucjlsbjxz => eymoie, dr => nmicjny, pguj => hrtetkxlmj);
  
  -- Single-driven assignments
  uhxfvrzk <= 'b';
  
  -- Multi-driven assignments
  eymoie <= "";
end tntk;

entity nhdmyjjfew is
  port (xx : out real);
end nhdmyjjfew;

library ieee;
use ieee.std_logic_1164.all;

architecture jyocc of nhdmyjjfew is
  signal sekzm : real;
  signal yrpvrj : std_logic_vector(0 downto 2);
begin
  cleazpxaye : entity work.kkwxbkh
    port map (aucjlsbjxz => yrpvrj, dr => xx, pguj => sekzm);
  
  -- Multi-driven assignments
  yrpvrj <= (others => '0');
  yrpvrj <= "";
end jyocc;

library ieee;
use ieee.std_logic_1164.all;

entity jebhy is
  port (fvzvje : buffer std_logic_vector(2 to 0); ugezd : linkage character; lvzquim : inout real);
end jebhy;

library ieee;
use ieee.std_logic_1164.all;

architecture egzrhdbo of jebhy is
  signal n : real;
  signal skal : real;
  signal bg : std_logic_vector(0 downto 2);
  signal agwlwohmh : real;
  signal tqp : std_logic_vector(0 downto 2);
begin
  dndrkg : entity work.kkwxbkh
    port map (aucjlsbjxz => tqp, dr => lvzquim, pguj => agwlwohmh);
  iaqt : entity work.kkwxbkh
    port map (aucjlsbjxz => bg, dr => skal, pguj => n);
  
  -- Multi-driven assignments
  fvzvje <= (others => '0');
  bg <= (others => '0');
  fvzvje <= "";
end egzrhdbo;



-- Seed after: 2499732910859147114,14629254427735353553
