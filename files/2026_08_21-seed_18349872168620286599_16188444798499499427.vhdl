-- Seed: 18349872168620286599,16188444798499499427

entity prwzrjqay is
  port (xyjgc : buffer boolean_vector(4 downto 1));
end prwzrjqay;

architecture pg of prwzrjqay is
  
begin
  -- Single-driven assignments
  xyjgc <= (TRUE, FALSE, TRUE, TRUE);
end pg;

library ieee;
use ieee.std_logic_1164.all;

entity povhj is
  port (rycerjvd : in time; vkcafxttk : linkage std_logic; zmlw : inout integer);
end povhj;

architecture mrz of povhj is
  signal gcwapd : boolean_vector(4 downto 1);
begin
  at : entity work.prwzrjqay
    port map (xyjgc => gcwapd);
  
  -- Single-driven assignments
  zmlw <= 4_2_1_1;
end mrz;

library ieee;
use ieee.std_logic_1164.all;

entity gt is
  port (nug : out std_logic);
end gt;

library ieee;
use ieee.std_logic_1164.all;

architecture ohiq of gt is
  signal baadimg : integer;
  signal hoxuoiwfqk : std_logic;
  signal jtyeftx : time;
  signal c : integer;
  signal oj : std_logic;
  signal ltligkkgm : time;
begin
  hqwd : entity work.povhj
    port map (rycerjvd => ltligkkgm, vkcafxttk => oj, zmlw => c);
  xkya : entity work.povhj
    port map (rycerjvd => jtyeftx, vkcafxttk => hoxuoiwfqk, zmlw => baadimg);
  
  -- Single-driven assignments
  ltligkkgm <= 8#1.2_4_6_5_1# ns;
  jtyeftx <= ltligkkgm;
end ohiq;



-- Seed after: 9628321705356383778,16188444798499499427
