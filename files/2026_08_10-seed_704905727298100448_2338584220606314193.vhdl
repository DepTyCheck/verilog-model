-- Seed: 704905727298100448,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity hahdq is
  port (umtxrjgqae : out boolean; i : linkage std_logic_vector(4 to 4));
end hahdq;

architecture ajf of hahdq is
  
begin
  -- Single-driven assignments
  umtxrjgqae <= TRUE;
end ajf;

entity lpxrwr is
  port (kbjfztsv : linkage boolean);
end lpxrwr;

library ieee;
use ieee.std_logic_1164.all;

architecture anxkfn of lpxrwr is
  signal tzznittk : std_logic_vector(4 to 4);
  signal ntkismvc : boolean;
begin
  yeqlndske : entity work.hahdq
    port map (umtxrjgqae => ntkismvc, i => tzznittk);
  
  -- Multi-driven assignments
  tzznittk <= tzznittk;
  tzznittk <= tzznittk;
  tzznittk <= (others => 'H');
  tzznittk <= "H";
end anxkfn;

entity uavic is
  port (pcw : buffer real; fd : in string(2 downto 5); ml : out severity_level; zgqmaedsv : in integer);
end uavic;

architecture nyfdz of uavic is
  signal rwehfw : boolean;
begin
  dr : entity work.lpxrwr
    port map (kbjfztsv => rwehfw);
end nyfdz;

library ieee;
use ieee.std_logic_1164.all;

entity hb is
  port (yeh : buffer real; hbblw : in std_logic; qbahnkuw : in time; jkh : linkage real);
end hb;

library ieee;
use ieee.std_logic_1164.all;

architecture ti of hb is
  signal mnzrbye : std_logic_vector(4 to 4);
  signal rmzk : boolean;
  signal ebjsdbcvqe : integer;
  signal igerui : severity_level;
  signal mbqhor : string(2 downto 5);
  signal njmiz : boolean;
begin
  umkn : entity work.lpxrwr
    port map (kbjfztsv => njmiz);
  yr : entity work.uavic
    port map (pcw => yeh, fd => mbqhor, ml => igerui, zgqmaedsv => ebjsdbcvqe);
  i : entity work.hahdq
    port map (umtxrjgqae => rmzk, i => mnzrbye);
  
  -- Single-driven assignments
  ebjsdbcvqe <= ebjsdbcvqe;
  mbqhor <= (others => ' ');
end ti;



-- Seed after: 7952248209856214723,2338584220606314193
