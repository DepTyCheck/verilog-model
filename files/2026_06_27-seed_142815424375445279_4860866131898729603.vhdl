-- Seed: 142815424375445279,4860866131898729603

entity jv is
  port (kg : out boolean_vector(0 downto 2); ydudnse : linkage real; jzitcbee : buffer time; wslhmkztx : out time_vector(3 to 0));
end jv;

architecture dxcqlo of jv is
  
begin
  -- Single-driven assignments
  wslhmkztx <= (others => 0 ns);
  kg <= (others => TRUE);
  jzitcbee <= 00.2_2 ms;
end dxcqlo;

library ieee;
use ieee.std_logic_1164.all;

entity yxlqdnyu is
  port (pnk : linkage std_logic_vector(2 downto 4); a : out std_logic; ohdqqahsdx : in std_logic);
end yxlqdnyu;

architecture svyrf of yxlqdnyu is
  signal skaspoxrg : time_vector(3 to 0);
  signal moqawzz : time;
  signal bamwrkd : real;
  signal pa : boolean_vector(0 downto 2);
  signal tpg : time_vector(3 to 0);
  signal u : time;
  signal hth : real;
  signal pmmqmdmv : boolean_vector(0 downto 2);
begin
  cmbzrtd : entity work.jv
    port map (kg => pmmqmdmv, ydudnse => hth, jzitcbee => u, wslhmkztx => tpg);
  ufs : entity work.jv
    port map (kg => pa, ydudnse => bamwrkd, jzitcbee => moqawzz, wslhmkztx => skaspoxrg);
  
  -- Multi-driven assignments
  a <= 'W';
  a <= 'Z';
end svyrf;

entity hvqge is
  port (puf : linkage integer; etzyvio : buffer integer);
end hvqge;

architecture ptyzfoova of hvqge is
  signal fu : time_vector(3 to 0);
  signal soqwuszu : time;
  signal cbthnurc : real;
  signal whwv : boolean_vector(0 downto 2);
  signal ipx : time_vector(3 to 0);
  signal zy : time;
  signal qdwvgnkkxn : real;
  signal qwaxahgpw : boolean_vector(0 downto 2);
  signal zmgecdfqg : time_vector(3 to 0);
  signal qfcm : time;
  signal qvgbgydbv : real;
  signal itwxl : boolean_vector(0 downto 2);
  signal ezrxt : time_vector(3 to 0);
  signal j : time;
  signal rrwlxplal : real;
  signal ngvibota : boolean_vector(0 downto 2);
begin
  waydwl : entity work.jv
    port map (kg => ngvibota, ydudnse => rrwlxplal, jzitcbee => j, wslhmkztx => ezrxt);
  eonux : entity work.jv
    port map (kg => itwxl, ydudnse => qvgbgydbv, jzitcbee => qfcm, wslhmkztx => zmgecdfqg);
  cg : entity work.jv
    port map (kg => qwaxahgpw, ydudnse => qdwvgnkkxn, jzitcbee => zy, wslhmkztx => ipx);
  xnmkdcg : entity work.jv
    port map (kg => whwv, ydudnse => cbthnurc, jzitcbee => soqwuszu, wslhmkztx => fu);
  
  -- Single-driven assignments
  etzyvio <= 2#01010#;
end ptyzfoova;

library ieee;
use ieee.std_logic_1164.all;

entity tkfqqe is
  port (vk : out std_logic; mjytjxcq : linkage bit; bozlp : buffer integer; e : inout boolean_vector(4 downto 2));
end tkfqqe;

library ieee;
use ieee.std_logic_1164.all;

architecture xdqthjlq of tkfqqe is
  signal xueltvihx : std_logic;
  signal j : std_logic_vector(2 downto 4);
  signal btvdxcf : std_logic_vector(2 downto 4);
  signal silt : integer;
  signal xykncyl : integer;
begin
  pemg : entity work.hvqge
    port map (puf => xykncyl, etzyvio => silt);
  rnzytr : entity work.yxlqdnyu
    port map (pnk => btvdxcf, a => vk, ohdqqahsdx => vk);
  yhb : entity work.yxlqdnyu
    port map (pnk => j, a => vk, ohdqqahsdx => xueltvihx);
  
  -- Single-driven assignments
  bozlp <= 2#11#;
  e <= (FALSE, TRUE, TRUE);
  
  -- Multi-driven assignments
  vk <= '1';
  vk <= '-';
  vk <= 'H';
end xdqthjlq;



-- Seed after: 9593942343740238475,4860866131898729603
