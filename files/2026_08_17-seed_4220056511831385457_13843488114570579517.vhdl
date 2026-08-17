-- Seed: 4220056511831385457,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity tkbag is
  port (mntgukzo : buffer bit; tijwhikdn : inout std_logic_vector(1 to 0); ktwgiy : inout time; vft : inout time);
end tkbag;

architecture t of tkbag is
  
begin
  -- Single-driven assignments
  mntgukzo <= '1';
  vft <= vft;
  ktwgiy <= vft;
  
  -- Multi-driven assignments
  tijwhikdn <= (others => '0');
  tijwhikdn <= "";
end t;

entity tmbrv is
  port (ipuvolvk : buffer string(2 to 3));
end tmbrv;

library ieee;
use ieee.std_logic_1164.all;

architecture p of tmbrv is
  signal mdefupjm : time;
  signal x : time;
  signal mzbln : std_logic_vector(1 to 0);
  signal ockxw : bit;
  signal inis : time;
  signal inixiei : time;
  signal qfpazkbvu : std_logic_vector(1 to 0);
  signal vgf : bit;
begin
  yrzddfuvty : entity work.tkbag
    port map (mntgukzo => vgf, tijwhikdn => qfpazkbvu, ktwgiy => inixiei, vft => inis);
  mygxyjgt : entity work.tkbag
    port map (mntgukzo => ockxw, tijwhikdn => mzbln, ktwgiy => x, vft => mdefupjm);
  
  -- Multi-driven assignments
  mzbln <= qfpazkbvu;
  qfpazkbvu <= qfpazkbvu;
  qfpazkbvu <= qfpazkbvu;
  qfpazkbvu <= qfpazkbvu;
end p;



-- Seed after: 6710918184563038047,13843488114570579517
