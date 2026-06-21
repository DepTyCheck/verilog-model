-- Seed: 867831778537034631,3687118713772291287

entity oca is
  port (rv : out boolean);
end oca;

architecture h of oca is
  
begin
  -- Single-driven assignments
  rv <= TRUE;
end h;

entity ljxm is
  port (hdqda : out boolean_vector(3 downto 3); ilkhuifawb : out real);
end ljxm;

architecture vvdrrkd of ljxm is
  
begin
  
end vvdrrkd;

entity pq is
  port (a : linkage time);
end pq;

architecture htviwfjdt of pq is
  signal iapmc : real;
  signal bdyghi : boolean_vector(3 downto 3);
  signal ix : boolean;
  signal le : real;
  signal fyuxmpf : boolean_vector(3 downto 3);
  signal jmuom : boolean;
begin
  voekrjvjv : entity work.oca
    port map (rv => jmuom);
  sjwgknhe : entity work.ljxm
    port map (hdqda => fyuxmpf, ilkhuifawb => le);
  bwxkd : entity work.oca
    port map (rv => ix);
  iom : entity work.ljxm
    port map (hdqda => bdyghi, ilkhuifawb => iapmc);
end htviwfjdt;

library ieee;
use ieee.std_logic_1164.all;

entity xryt is
  port (axbtld : inout std_logic_vector(4 to 2); zrbcqq : buffer integer; j : inout boolean);
end xryt;

architecture dispsarcq of xryt is
  signal h : real;
  signal zhpenqnjjj : boolean_vector(3 downto 3);
begin
  irskvvht : entity work.ljxm
    port map (hdqda => zhpenqnjjj, ilkhuifawb => h);
  edns : entity work.oca
    port map (rv => j);
  
  -- Single-driven assignments
  zrbcqq <= 2#0_1_1#;
end dispsarcq;



-- Seed after: 16906122137064481460,3687118713772291287
