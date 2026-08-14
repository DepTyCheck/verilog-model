-- Seed: 6862196735655659776,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity kiolnigtub is
  port (esvqe : inout std_logic; dxif : linkage time_vector(3 downto 1); cufqcxrm : linkage std_logic);
end kiolnigtub;

architecture jtcjvirxz of kiolnigtub is
  
begin
  -- Multi-driven assignments
  esvqe <= 'Z';
  esvqe <= esvqe;
  esvqe <= esvqe;
  esvqe <= esvqe;
end jtcjvirxz;

library ieee;
use ieee.std_logic_1164.all;

entity dy is
  port (m : linkage time; pnanl : buffer std_logic; cewkcrj : out integer);
end dy;

library ieee;
use ieee.std_logic_1164.all;

architecture bsvvcch of dy is
  signal fbgthbdakx : std_logic;
  signal tggvgi : time_vector(3 downto 1);
  signal agfbf : std_logic;
  signal ilmgpqyxsc : time_vector(3 downto 1);
  signal rrwvspn : std_logic;
  signal wilojffkk : time_vector(3 downto 1);
  signal innza : std_logic;
  signal aqtpcjorxn : time_vector(3 downto 1);
  signal cxeleend : std_logic;
begin
  acjrkrjc : entity work.kiolnigtub
    port map (esvqe => cxeleend, dxif => aqtpcjorxn, cufqcxrm => cxeleend);
  uhxfqnfhi : entity work.kiolnigtub
    port map (esvqe => innza, dxif => wilojffkk, cufqcxrm => cxeleend);
  qocfr : entity work.kiolnigtub
    port map (esvqe => rrwvspn, dxif => ilmgpqyxsc, cufqcxrm => pnanl);
  b : entity work.kiolnigtub
    port map (esvqe => agfbf, dxif => tggvgi, cufqcxrm => fbgthbdakx);
  
  -- Single-driven assignments
  cewkcrj <= 013;
  
  -- Multi-driven assignments
  pnanl <= 'Z';
  pnanl <= 'H';
end bsvvcch;

library ieee;
use ieee.std_logic_1164.all;

entity hvzxn is
  port (ntee : in integer_vector(2 downto 3); zbbizkmn : buffer std_logic);
end hvzxn;

library ieee;
use ieee.std_logic_1164.all;

architecture ochalu of hvzxn is
  signal oef : std_logic;
  signal rmsymrxyza : time_vector(3 downto 1);
  signal bqpd : std_logic;
  signal gxmvw : time_vector(3 downto 1);
  signal cbfk : time_vector(3 downto 1);
  signal wvmbyqkqad : std_logic;
  signal hxp : time_vector(3 downto 1);
  signal a : std_logic;
begin
  ndo : entity work.kiolnigtub
    port map (esvqe => a, dxif => hxp, cufqcxrm => wvmbyqkqad);
  mx : entity work.kiolnigtub
    port map (esvqe => zbbizkmn, dxif => cbfk, cufqcxrm => zbbizkmn);
  iilhsnrvod : entity work.kiolnigtub
    port map (esvqe => wvmbyqkqad, dxif => gxmvw, cufqcxrm => zbbizkmn);
  yrsczegp : entity work.kiolnigtub
    port map (esvqe => bqpd, dxif => rmsymrxyza, cufqcxrm => oef);
  
  -- Multi-driven assignments
  zbbizkmn <= a;
end ochalu;

entity hfrbwux is
  port (geatvwt : out time; ow : buffer time);
end hfrbwux;

library ieee;
use ieee.std_logic_1164.all;

architecture y of hfrbwux is
  signal nfwo : std_logic;
  signal va : time_vector(3 downto 1);
  signal okhulhynrr : std_logic;
begin
  gb : entity work.kiolnigtub
    port map (esvqe => okhulhynrr, dxif => va, cufqcxrm => nfwo);
  
  -- Single-driven assignments
  geatvwt <= 2_3 ms;
  ow <= 2#0.1_1# fs;
end y;



-- Seed after: 11300587892918268013,8437298063418820479
