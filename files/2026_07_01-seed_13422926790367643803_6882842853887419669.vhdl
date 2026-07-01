-- Seed: 13422926790367643803,6882842853887419669

entity gdufoyxi is
  port (j : out bit; nz : buffer time);
end gdufoyxi;

architecture oglsk of gdufoyxi is
  
begin
  -- Single-driven assignments
  nz <= 8#1.2# us;
  j <= '1';
end oglsk;

entity axkvxfl is
  port (dyhq : in boolean);
end axkvxfl;

architecture ztj of axkvxfl is
  signal rymntw : time;
  signal etvufuq : bit;
  signal os : time;
  signal bolesrs : bit;
begin
  ots : entity work.gdufoyxi
    port map (j => bolesrs, nz => os);
  tupfeo : entity work.gdufoyxi
    port map (j => etvufuq, nz => rymntw);
end ztj;

library ieee;
use ieee.std_logic_1164.all;

entity twbab is
  port (iypjjhr : out std_logic_vector(4 downto 4); yoyestb : buffer integer);
end twbab;

architecture wubwmco of twbab is
  signal onuc : time;
  signal kcm : bit;
begin
  mcghoq : entity work.gdufoyxi
    port map (j => kcm, nz => onuc);
  
  -- Single-driven assignments
  yoyestb <= 1_1_4;
  
  -- Multi-driven assignments
  iypjjhr <= (others => 'X');
end wubwmco;



-- Seed after: 5513074963426166765,6882842853887419669
