-- Seed: 4755718293293979476,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity exzvvat is
  port (bg : inout std_logic_vector(1 downto 1); aeiozuv : linkage std_logic_vector(4 to 2));
end exzvvat;

architecture uykyicgb of exzvvat is
  
begin
  -- Multi-driven assignments
  bg <= "X";
  bg <= bg;
end uykyicgb;

entity xi is
  port (apuxmszuqi : in time; xwmulplyb : in time; umc : inout time_vector(1 downto 4));
end xi;

library ieee;
use ieee.std_logic_1164.all;

architecture hk of xi is
  signal zzzhq : std_logic_vector(4 to 2);
  signal unh : std_logic_vector(1 downto 1);
  signal ejvbabgxb : std_logic_vector(4 to 2);
  signal z : std_logic_vector(1 downto 1);
begin
  keoy : entity work.exzvvat
    port map (bg => z, aeiozuv => ejvbabgxb);
  lpsxspzfh : entity work.exzvvat
    port map (bg => z, aeiozuv => ejvbabgxb);
  i : entity work.exzvvat
    port map (bg => unh, aeiozuv => zzzhq);
  
  -- Single-driven assignments
  umc <= umc;
  
  -- Multi-driven assignments
  z <= (others => 'U');
  z <= (others => '0');
  ejvbabgxb <= "";
end hk;

library ieee;
use ieee.std_logic_1164.all;

entity hydaqybmg is
  port (fbfabwapy : inout std_logic_vector(2 to 1); ssouqq : inout real);
end hydaqybmg;

architecture xwqrid of hydaqybmg is
  
begin
  -- Multi-driven assignments
  fbfabwapy <= (others => '0');
  fbfabwapy <= "";
  fbfabwapy <= "";
  fbfabwapy <= "";
end xwqrid;

library ieee;
use ieee.std_logic_1164.all;

entity te is
  port (kolisr : linkage std_logic);
end te;

library ieee;
use ieee.std_logic_1164.all;

architecture dryyogv of te is
  signal s : std_logic_vector(1 downto 1);
  signal nmjtl : real;
  signal ceguszmu : std_logic_vector(4 to 2);
  signal bnbd : time_vector(1 downto 4);
  signal vlu : time;
  signal gedv : time;
begin
  bawjvl : entity work.xi
    port map (apuxmszuqi => gedv, xwmulplyb => vlu, umc => bnbd);
  otuxlpakhw : entity work.hydaqybmg
    port map (fbfabwapy => ceguszmu, ssouqq => nmjtl);
  k : entity work.exzvvat
    port map (bg => s, aeiozuv => ceguszmu);
  
  -- Single-driven assignments
  gedv <= 2_1.23 us;
  
  -- Multi-driven assignments
  ceguszmu <= ceguszmu;
end dryyogv;



-- Seed after: 9258964375518953751,13592003931158285879
