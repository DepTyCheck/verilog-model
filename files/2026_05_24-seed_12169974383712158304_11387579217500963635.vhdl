-- Seed: 12169974383712158304,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity effyc is
  port (kmpzfmq : in std_logic; sywk : linkage real; mw : linkage std_logic_vector(0 to 3));
end effyc;



architecture ufupjj of effyc is
  
begin
  
end ufupjj;



entity hu is
  port (xsqwiqkl : inout bit_vector(4 downto 1); wpi : out bit_vector(4 to 1));
end hu;

library ieee;
use ieee.std_logic_1164.all;

architecture iqqaqhbujg of hu is
  signal yyqmsp : std_logic_vector(0 to 3);
  signal cbbujnvke : real;
  signal uwvzz : std_logic;
  signal zs : std_logic_vector(0 to 3);
  signal hqpgq : real;
  signal heellpavg : std_logic;
begin
  j : entity work.effyc
    port map (kmpzfmq => heellpavg, sywk => hqpgq, mw => zs);
  ytltapjdze : entity work.effyc
    port map (kmpzfmq => uwvzz, sywk => hqpgq, mw => zs);
  dv : entity work.effyc
    port map (kmpzfmq => heellpavg, sywk => cbbujnvke, mw => yyqmsp);
end iqqaqhbujg;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (gxmjrkbdpd : inout integer; eij : buffer std_logic_vector(1 downto 2));
end r;



architecture vkxfzl of r is
  
begin
  
end vkxfzl;

library ieee;
use ieee.std_logic_1164.all;

entity xcoo is
  port (cofqfkanc : buffer std_logic; zoexf : buffer std_logic_vector(0 to 4));
end xcoo;

library ieee;
use ieee.std_logic_1164.all;

architecture z of xcoo is
  signal svkdwwnm : std_logic_vector(1 downto 2);
  signal juxlz : integer;
  signal zkraxwr : integer;
  signal jjvfhgqlb : std_logic_vector(0 to 3);
  signal myqmjaoeo : real;
  signal lyicr : std_logic_vector(1 downto 2);
  signal pbi : integer;
begin
  zbvhmjgmv : entity work.r
    port map (gxmjrkbdpd => pbi, eij => lyicr);
  khgyyhtzc : entity work.effyc
    port map (kmpzfmq => cofqfkanc, sywk => myqmjaoeo, mw => jjvfhgqlb);
  tyey : entity work.r
    port map (gxmjrkbdpd => zkraxwr, eij => lyicr);
  ia : entity work.r
    port map (gxmjrkbdpd => juxlz, eij => svkdwwnm);
end z;



-- Seed after: 17841267593759521804,11387579217500963635
