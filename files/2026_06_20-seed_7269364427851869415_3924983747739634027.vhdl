-- Seed: 7269364427851869415,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity pddruqf is
  port (hns : out std_logic_vector(1 to 0); nouhlhjp : buffer real);
end pddruqf;

architecture aufxa of pddruqf is
  
begin
  -- Single-driven assignments
  nouhlhjp <= 40.4_2_1;
end aufxa;

entity zcd is
  port (km : out bit);
end zcd;

library ieee;
use ieee.std_logic_1164.all;

architecture pxu of zcd is
  signal stlgcsdfuk : real;
  signal oihf : real;
  signal koghsnjpo : std_logic_vector(1 to 0);
begin
  iqz : entity work.pddruqf
    port map (hns => koghsnjpo, nouhlhjp => oihf);
  qi : entity work.pddruqf
    port map (hns => koghsnjpo, nouhlhjp => stlgcsdfuk);
  
  -- Single-driven assignments
  km <= '1';
  
  -- Multi-driven assignments
  koghsnjpo <= (others => '0');
end pxu;

entity lrnoxod is
  port (qviq : out real_vector(2 to 2); hataovadhv : in severity_level);
end lrnoxod;

library ieee;
use ieee.std_logic_1164.all;

architecture xtrkn of lrnoxod is
  signal wuoqo : real;
  signal qc : std_logic_vector(1 to 0);
begin
  jowstortvd : entity work.pddruqf
    port map (hns => qc, nouhlhjp => wuoqo);
  
  -- Multi-driven assignments
  qc <= (others => '0');
end xtrkn;

entity lhfiw is
  port (fibvsj : buffer integer_vector(3 downto 4); yybrzjbcg : inout time);
end lhfiw;

library ieee;
use ieee.std_logic_1164.all;

architecture dft of lhfiw is
  signal urja : bit;
  signal nv : real;
  signal apsvjutzg : std_logic_vector(1 to 0);
begin
  roxvod : entity work.pddruqf
    port map (hns => apsvjutzg, nouhlhjp => nv);
  vdpalsupu : entity work.zcd
    port map (km => urja);
  
  -- Single-driven assignments
  yybrzjbcg <= 2_3_1_4 ps;
  fibvsj <= (others => 0);
  
  -- Multi-driven assignments
  apsvjutzg <= "";
  apsvjutzg <= (others => '0');
  apsvjutzg <= (others => '0');
  apsvjutzg <= (others => '0');
end dft;



-- Seed after: 16359217509515304564,3924983747739634027
