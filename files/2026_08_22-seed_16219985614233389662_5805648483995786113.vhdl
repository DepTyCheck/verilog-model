-- Seed: 16219985614233389662,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity ippz is
  port (ctzn : in std_logic; lscj : out character; qfbpsr : linkage std_logic);
end ippz;

architecture dcuq of ippz is
  
begin
  
end dcuq;

library ieee;
use ieee.std_logic_1164.all;

entity edb is
  port (dy : buffer real; fweownx : in std_logic; txuvgvrfd : out bit_vector(0 downto 1));
end edb;

library ieee;
use ieee.std_logic_1164.all;

architecture ewv of edb is
  signal omvx : character;
  signal kcjihuxa : character;
  signal rzmufab : std_logic;
  signal rf : std_logic;
  signal llrerzqh : character;
  signal jufirpro : std_logic;
begin
  yukfij : entity work.ippz
    port map (ctzn => jufirpro, lscj => llrerzqh, qfbpsr => rf);
  gfiw : entity work.ippz
    port map (ctzn => rzmufab, lscj => kcjihuxa, qfbpsr => fweownx);
  rvskzhvyb : entity work.ippz
    port map (ctzn => rf, lscj => omvx, qfbpsr => jufirpro);
  
  -- Single-driven assignments
  txuvgvrfd <= (others => '0');
  dy <= 8#1_7.366#;
  
  -- Multi-driven assignments
  rzmufab <= jufirpro;
  rzmufab <= 'W';
  rf <= rf;
end ewv;

entity nh is
  port (olvgtfs : in integer);
end nh;

library ieee;
use ieee.std_logic_1164.all;

architecture b of nh is
  signal wvuggvcxjj : character;
  signal thxyicpi : std_logic;
  signal mc : character;
  signal v : std_logic;
  signal qycvokb : bit_vector(0 downto 1);
  signal dfcveqfbwo : std_logic;
  signal ywdqxnjbet : real;
  signal ntbrbtclro : bit_vector(0 downto 1);
  signal phkkt : std_logic;
  signal uzowjafzu : real;
begin
  djdvhafvr : entity work.edb
    port map (dy => uzowjafzu, fweownx => phkkt, txuvgvrfd => ntbrbtclro);
  onnqt : entity work.edb
    port map (dy => ywdqxnjbet, fweownx => dfcveqfbwo, txuvgvrfd => qycvokb);
  lyyc : entity work.ippz
    port map (ctzn => v, lscj => mc, qfbpsr => phkkt);
  mrwbbegtvp : entity work.ippz
    port map (ctzn => thxyicpi, lscj => wvuggvcxjj, qfbpsr => dfcveqfbwo);
  
  -- Multi-driven assignments
  phkkt <= phkkt;
end b;



-- Seed after: 12089280402000958818,5805648483995786113
