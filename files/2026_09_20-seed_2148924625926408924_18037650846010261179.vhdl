-- Seed: 2148924625926408924,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity hsbc is
  port (nn : in real; byo : linkage bit; qnsxo : linkage std_logic_vector(1 to 2));
end hsbc;

architecture zqnbgbixb of hsbc is
  
begin
  
end zqnbgbixb;

library ieee;
use ieee.std_logic_1164.all;

entity hcvtdc is
  port (vnchslgz : in std_logic);
end hcvtdc;

library ieee;
use ieee.std_logic_1164.all;

architecture xirhxczokk of hcvtdc is
  signal pjxt : bit;
  signal gho : bit;
  signal lrq : real;
  signal gyp : std_logic_vector(1 to 2);
  signal u : bit;
  signal rxrzofff : real;
begin
  zodgvpvmt : entity work.hsbc
    port map (nn => rxrzofff, byo => u, qnsxo => gyp);
  bdhp : entity work.hsbc
    port map (nn => lrq, byo => gho, qnsxo => gyp);
  m : entity work.hsbc
    port map (nn => rxrzofff, byo => pjxt, qnsxo => gyp);
  
  -- Single-driven assignments
  rxrzofff <= 16#341.1#;
  lrq <= 16#E_5_2_C.9#;
  
  -- Multi-driven assignments
  gyp <= ('-', 'L');
  gyp <= ('W', '-');
end xirhxczokk;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (qnxrqxn : out integer; yx : buffer time; sdx : buffer bit_vector(1 downto 4); d : in std_logic_vector(0 to 3));
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture rzuuyhqwb of v is
  signal mgy : std_logic_vector(1 to 2);
  signal hrb : bit;
  signal vokaqis : real;
begin
  jsnhp : entity work.hsbc
    port map (nn => vokaqis, byo => hrb, qnsxo => mgy);
  
  -- Single-driven assignments
  yx <= 33 ms;
  vokaqis <= vokaqis;
  qnxrqxn <= qnxrqxn;
  sdx <= (others => '0');
  
  -- Multi-driven assignments
  mgy <= "10";
  mgy <= "-X";
end rzuuyhqwb;

library ieee;
use ieee.std_logic_1164.all;

entity stnpkqvetd is
  port (edcpfqa : buffer boolean_vector(1 downto 3); cbptjpsw : linkage real_vector(1 downto 3); meazk : inout std_logic);
end stnpkqvetd;

library ieee;
use ieee.std_logic_1164.all;

architecture bo of stnpkqvetd is
  signal le : std_logic_vector(0 to 3);
  signal lt : bit_vector(1 downto 4);
  signal chb : time;
  signal vbealyvh : integer;
  signal b : std_logic_vector(1 to 2);
  signal mdvajhzd : bit;
  signal vox : real;
begin
  anpzuaqyg : entity work.hsbc
    port map (nn => vox, byo => mdvajhzd, qnsxo => b);
  ckysacazy : entity work.v
    port map (qnxrqxn => vbealyvh, yx => chb, sdx => lt, d => le);
  
  -- Single-driven assignments
  edcpfqa <= edcpfqa;
  vox <= 0.40;
  
  -- Multi-driven assignments
  meazk <= 'W';
  meazk <= meazk;
  meazk <= '-';
end bo;



-- Seed after: 846383201045819265,18037650846010261179
