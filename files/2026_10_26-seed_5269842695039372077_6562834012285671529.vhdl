-- Seed: 5269842695039372077,6562834012285671529

entity ruv is
  port (cgeyqb : in bit);
end ruv;

architecture fquvl of ruv is
  
begin
  
end fquvl;

entity nhyo is
  port (zjv : in string(1 to 3));
end nhyo;

architecture ntsqp of nhyo is
  signal glybh : bit;
begin
  bfkszzketz : entity work.ruv
    port map (cgeyqb => glybh);
  
  -- Single-driven assignments
  glybh <= '1';
end ntsqp;

library ieee;
use ieee.std_logic_1164.all;

entity pkwqhjtlp is
  port (rhknypmo : buffer std_logic; gliokidwd : in boolean_vector(0 to 1));
end pkwqhjtlp;

architecture itx of pkwqhjtlp is
  signal dhu : bit;
  signal colqvsx : bit;
begin
  it : entity work.ruv
    port map (cgeyqb => colqvsx);
  mrx : entity work.ruv
    port map (cgeyqb => colqvsx);
  bz : entity work.ruv
    port map (cgeyqb => dhu);
  ushrolgbjt : entity work.ruv
    port map (cgeyqb => colqvsx);
  
  -- Single-driven assignments
  colqvsx <= colqvsx;
  dhu <= colqvsx;
  
  -- Multi-driven assignments
  rhknypmo <= rhknypmo;
end itx;

entity wexbecsr is
  port (imqcmefwmz : inout time; lodzxcgy : in integer; hdfntoi : inout time; jqrv : buffer real_vector(4 downto 1));
end wexbecsr;

library ieee;
use ieee.std_logic_1164.all;

architecture sjetx of wexbecsr is
  signal jowztergd : bit;
  signal y : boolean_vector(0 to 1);
  signal ptlqsojz : std_logic;
begin
  aoufskn : entity work.pkwqhjtlp
    port map (rhknypmo => ptlqsojz, gliokidwd => y);
  hfhiji : entity work.ruv
    port map (cgeyqb => jowztergd);
  
  -- Multi-driven assignments
  ptlqsojz <= ptlqsojz;
  ptlqsojz <= '1';
  ptlqsojz <= ptlqsojz;
end sjetx;



-- Seed after: 11067317493696439352,6562834012285671529
