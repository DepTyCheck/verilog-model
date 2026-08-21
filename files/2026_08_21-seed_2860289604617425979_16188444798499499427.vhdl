-- Seed: 2860289604617425979,16188444798499499427

entity vp is
  port (dczyhxb : out character; odywdt : buffer time; ianywazt : inout severity_level);
end vp;

architecture t of vp is
  
begin
  -- Single-driven assignments
  ianywazt <= WARNING;
end t;

entity cirxfzhi is
  port (sp : in time; ebwwp : inout time);
end cirxfzhi;

architecture bvfftan of cirxfzhi is
  signal bkxe : severity_level;
  signal ycgqzb : time;
  signal drjxh : character;
  signal kz : severity_level;
  signal uawpuzrcad : time;
  signal o : character;
  signal rytoq : severity_level;
  signal yvpjb : time;
  signal zqlvzrit : character;
begin
  pdhvythdov : entity work.vp
    port map (dczyhxb => zqlvzrit, odywdt => yvpjb, ianywazt => rytoq);
  jou : entity work.vp
    port map (dczyhxb => o, odywdt => uawpuzrcad, ianywazt => kz);
  iordckutdo : entity work.vp
    port map (dczyhxb => drjxh, odywdt => ycgqzb, ianywazt => bkxe);
  
  -- Single-driven assignments
  ebwwp <= ebwwp;
end bvfftan;

library ieee;
use ieee.std_logic_1164.all;

entity iilaqll is
  port (xocv : inout std_logic);
end iilaqll;

architecture nxqwknm of iilaqll is
  signal ybuu : time;
  signal rlrbx : severity_level;
  signal cjigeejfiw : time;
  signal bsvgzefm : character;
begin
  ipfbnzch : entity work.vp
    port map (dczyhxb => bsvgzefm, odywdt => cjigeejfiw, ianywazt => rlrbx);
  ky : entity work.cirxfzhi
    port map (sp => cjigeejfiw, ebwwp => ybuu);
end nxqwknm;



-- Seed after: 14988379989689260997,16188444798499499427
