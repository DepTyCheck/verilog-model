-- Seed: 15426074700492667111,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity znyl is
  port (swzuwgwk : out real; etdnm : linkage std_logic_vector(3 downto 2));
end znyl;

architecture ugqvbgtl of znyl is
  
begin
  -- Single-driven assignments
  swzuwgwk <= 8#36.52665#;
end ugqvbgtl;

library ieee;
use ieee.std_logic_1164.all;

entity hzkkv is
  port (ebc : in integer_vector(0 downto 1); ipbpz : in std_logic_vector(4 downto 2); khydcswhdd : linkage integer);
end hzkkv;

library ieee;
use ieee.std_logic_1164.all;

architecture ib of hzkkv is
  signal qebxxkobv : real;
  signal mkznxot : real;
  signal v : std_logic_vector(3 downto 2);
  signal uozjzr : real;
  signal q : std_logic_vector(3 downto 2);
  signal emrh : real;
begin
  dfri : entity work.znyl
    port map (swzuwgwk => emrh, etdnm => q);
  m : entity work.znyl
    port map (swzuwgwk => uozjzr, etdnm => v);
  bniuf : entity work.znyl
    port map (swzuwgwk => mkznxot, etdnm => q);
  uridlr : entity work.znyl
    port map (swzuwgwk => qebxxkobv, etdnm => q);
  
  -- Multi-driven assignments
  q <= ('U', 'Z');
  q <= q;
  q <= q;
  q <= ('H', 'W');
end ib;

library ieee;
use ieee.std_logic_1164.all;

entity qlwzzwkw is
  port (ijupy : out integer; rzxsldi : linkage time; faav : buffer std_logic_vector(2 downto 3));
end qlwzzwkw;

library ieee;
use ieee.std_logic_1164.all;

architecture ogcqzkw of qlwzzwkw is
  signal sxxfurl : std_logic_vector(3 downto 2);
  signal eloptffmul : real;
  signal ecrg : std_logic_vector(3 downto 2);
  signal ldhx : real;
  signal bjgxllqipo : std_logic_vector(3 downto 2);
  signal c : real;
  signal vh : std_logic_vector(4 downto 2);
  signal uaewu : integer_vector(0 downto 1);
begin
  kp : entity work.hzkkv
    port map (ebc => uaewu, ipbpz => vh, khydcswhdd => ijupy);
  jlrkzdoea : entity work.znyl
    port map (swzuwgwk => c, etdnm => bjgxllqipo);
  eilwsuq : entity work.znyl
    port map (swzuwgwk => ldhx, etdnm => ecrg);
  ix : entity work.znyl
    port map (swzuwgwk => eloptffmul, etdnm => sxxfurl);
  
  -- Single-driven assignments
  uaewu <= uaewu;
end ogcqzkw;



-- Seed after: 3896028433254940447,8891552411914730853
