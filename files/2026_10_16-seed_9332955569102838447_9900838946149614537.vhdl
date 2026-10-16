-- Seed: 9332955569102838447,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity tdq is
  port (zs : out std_logic_vector(0 to 4); rslusqlca : inout std_logic; q : inout string(4 downto 3));
end tdq;

architecture vhbincbxeg of tdq is
  
begin
  -- Single-driven assignments
  q <= "xi";
  
  -- Multi-driven assignments
  rslusqlca <= 'Z';
  rslusqlca <= '1';
end vhbincbxeg;

entity ffba is
  port (y : buffer real; duqd : buffer real);
end ffba;

library ieee;
use ieee.std_logic_1164.all;

architecture hpbb of ffba is
  signal ghej : string(4 downto 3);
  signal h : std_logic;
  signal mxbocuf : std_logic_vector(0 to 4);
  signal xq : string(4 downto 3);
  signal kwdxdwru : std_logic;
  signal cbnn : std_logic_vector(0 to 4);
begin
  tykditf : entity work.tdq
    port map (zs => cbnn, rslusqlca => kwdxdwru, q => xq);
  k : entity work.tdq
    port map (zs => mxbocuf, rslusqlca => h, q => ghej);
  
  -- Single-driven assignments
  duqd <= 2_4_4_1_3.4;
  y <= duqd;
  
  -- Multi-driven assignments
  cbnn <= "ZHH1H";
end hpbb;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (dmsilk : in integer; faqhdonv : linkage std_logic);
end y;

architecture xt of y is
  
begin
  
end xt;

library ieee;
use ieee.std_logic_1164.all;

entity bpwcrvdcn is
  port (ghzoiiukkk : out real; eekrjhfii : out std_logic_vector(2 downto 3));
end bpwcrvdcn;

library ieee;
use ieee.std_logic_1164.all;

architecture l of bpwcrvdcn is
  signal qfejxck : string(4 downto 3);
  signal ak : std_logic;
  signal mvc : std_logic_vector(0 to 4);
  signal ynzpjsf : std_logic;
  signal zwahdut : integer;
begin
  qrvik : entity work.y
    port map (dmsilk => zwahdut, faqhdonv => ynzpjsf);
  trwdvqen : entity work.tdq
    port map (zs => mvc, rslusqlca => ak, q => qfejxck);
end l;



-- Seed after: 14004677781195724789,9900838946149614537
