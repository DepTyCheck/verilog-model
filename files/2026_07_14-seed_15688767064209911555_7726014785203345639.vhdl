-- Seed: 15688767064209911555,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity bzgcqqz is
  port (johalhb : in time_vector(1 to 1); xntovtb : linkage std_logic);
end bzgcqqz;

architecture lav of bzgcqqz is
  
begin
  
end lav;

entity erwgz is
  port (zpiy : inout integer; lcafutwic : out integer);
end erwgz;

library ieee;
use ieee.std_logic_1164.all;

architecture knccvhqku of erwgz is
  signal djtbwy : std_logic;
  signal kkho : time_vector(1 to 1);
  signal wkms : std_logic;
  signal b : std_logic;
  signal bacjzkehw : time_vector(1 to 1);
  signal i : std_logic;
  signal ewbmhe : time_vector(1 to 1);
begin
  lulkibny : entity work.bzgcqqz
    port map (johalhb => ewbmhe, xntovtb => i);
  h : entity work.bzgcqqz
    port map (johalhb => bacjzkehw, xntovtb => b);
  lgif : entity work.bzgcqqz
    port map (johalhb => ewbmhe, xntovtb => wkms);
  nldhotj : entity work.bzgcqqz
    port map (johalhb => kkho, xntovtb => djtbwy);
  
  -- Single-driven assignments
  ewbmhe <= (others => 2#0_0.10# ns);
  lcafutwic <= 16#8_0_5_1_5#;
  
  -- Multi-driven assignments
  djtbwy <= wkms;
  b <= 'H';
  wkms <= i;
end knccvhqku;



-- Seed after: 4160329588315595848,7726014785203345639
