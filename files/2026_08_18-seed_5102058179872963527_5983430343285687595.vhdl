-- Seed: 5102058179872963527,5983430343285687595

entity fhvmsq is
  port (lrtaw : inout time_vector(3 to 4); awsxn : out bit);
end fhvmsq;

architecture srmveevyij of fhvmsq is
  
begin
  -- Single-driven assignments
  lrtaw <= (04 fs, 1 sec);
  awsxn <= '1';
end srmveevyij;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (p : in std_logic; lllndff : inout time; rlbtntyj : inout real);
end t;

architecture lriu of t is
  signal y : bit;
  signal ptrqniiyi : time_vector(3 to 4);
  signal joutyuir : bit;
  signal teexxyu : time_vector(3 to 4);
begin
  sbxlfcsfk : entity work.fhvmsq
    port map (lrtaw => teexxyu, awsxn => joutyuir);
  rlr : entity work.fhvmsq
    port map (lrtaw => ptrqniiyi, awsxn => y);
end lriu;

entity wvxbfz is
  port (agtmxesier : buffer time; pnxkliam : linkage integer; bwcmdksqc : linkage boolean; nswc : inout boolean_vector(0 downto 2));
end wvxbfz;

architecture mzr of wvxbfz is
  signal cndfft : bit;
  signal ktmgovjy : time_vector(3 to 4);
begin
  li : entity work.fhvmsq
    port map (lrtaw => ktmgovjy, awsxn => cndfft);
  
  -- Single-driven assignments
  agtmxesier <= agtmxesier;
end mzr;



-- Seed after: 16814410354606920158,5983430343285687595
