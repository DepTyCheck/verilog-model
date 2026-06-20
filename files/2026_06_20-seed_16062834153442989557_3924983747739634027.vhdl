-- Seed: 16062834153442989557,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity fmlptydk is
  port (xpmspcb : inout std_logic; cqj : inout std_logic_vector(0 downto 2); ualxte : out real);
end fmlptydk;

architecture oqoom of fmlptydk is
  
begin
  -- Single-driven assignments
  ualxte <= 24.4_2;
  
  -- Multi-driven assignments
  xpmspcb <= 'L';
  cqj <= "";
  cqj <= "";
  cqj <= "";
end oqoom;

entity pzda is
  port (ipvbkdf : out bit);
end pzda;

library ieee;
use ieee.std_logic_1164.all;

architecture rhu of pzda is
  signal grmxedirh : real;
  signal wjwuoyp : std_logic_vector(0 downto 2);
  signal dxvsud : real;
  signal g : std_logic_vector(0 downto 2);
  signal as : real;
  signal dqjpcy : std_logic_vector(0 downto 2);
  signal sogpv : std_logic;
  signal svparwvq : real;
  signal rwaem : std_logic_vector(0 downto 2);
  signal lnpuvycia : std_logic;
begin
  mqqlwdx : entity work.fmlptydk
    port map (xpmspcb => lnpuvycia, cqj => rwaem, ualxte => svparwvq);
  qfya : entity work.fmlptydk
    port map (xpmspcb => sogpv, cqj => dqjpcy, ualxte => as);
  ym : entity work.fmlptydk
    port map (xpmspcb => sogpv, cqj => g, ualxte => dxvsud);
  uiiojqp : entity work.fmlptydk
    port map (xpmspcb => lnpuvycia, cqj => wjwuoyp, ualxte => grmxedirh);
  
  -- Multi-driven assignments
  rwaem <= (others => '0');
  dqjpcy <= "";
  wjwuoyp <= "";
  g <= (others => '0');
end rhu;



-- Seed after: 17509703512384291179,3924983747739634027
