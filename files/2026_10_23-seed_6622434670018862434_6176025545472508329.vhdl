-- Seed: 6622434670018862434,6176025545472508329

entity ghxd is
  port (goqc : buffer time; tpu : out integer_vector(3 to 0); emjqj : in string(4 downto 2));
end ghxd;

architecture khfel of ghxd is
  
begin
  -- Single-driven assignments
  goqc <= 0.1_2_2 us;
  tpu <= (others => 0);
end khfel;

entity cing is
  port (jsuinh : buffer string(1 downto 3); trubws : in bit; d : in time);
end cing;

architecture dwugj of cing is
  signal lgatcscjnl : integer_vector(3 to 0);
  signal jzulkywp : time;
  signal iwwajspmj : string(4 downto 2);
  signal mwk : integer_vector(3 to 0);
  signal eg : time;
  signal gnrujje : string(4 downto 2);
  signal msyctvo : integer_vector(3 to 0);
  signal ka : time;
begin
  ygvzvph : entity work.ghxd
    port map (goqc => ka, tpu => msyctvo, emjqj => gnrujje);
  vw : entity work.ghxd
    port map (goqc => eg, tpu => mwk, emjqj => iwwajspmj);
  uv : entity work.ghxd
    port map (goqc => jzulkywp, tpu => lgatcscjnl, emjqj => gnrujje);
  
  -- Single-driven assignments
  iwwajspmj <= gnrujje;
  gnrujje <= gnrujje;
  jsuinh <= "";
end dwugj;

library ieee;
use ieee.std_logic_1164.all;

entity dmwaxyamda is
  port (bdmfktd : inout std_logic; z : inout std_logic);
end dmwaxyamda;

architecture louazgj of dmwaxyamda is
  signal a : string(4 downto 2);
  signal mqtbnvgeu : integer_vector(3 to 0);
  signal bcdmfqqtdn : time;
begin
  eoraaeu : entity work.ghxd
    port map (goqc => bcdmfqqtdn, tpu => mqtbnvgeu, emjqj => a);
  
  -- Single-driven assignments
  a <= a;
  
  -- Multi-driven assignments
  z <= '-';
  z <= z;
  z <= 'U';
end louazgj;

entity eqg is
  port (twjzos : out time; ict : out severity_level; ygtqdxl : in time_vector(0 downto 4));
end eqg;

architecture jwjoqi of eqg is
  signal fccp : bit;
  signal vec : string(1 downto 3);
  signal hadxuxr : integer_vector(3 to 0);
  signal olxqg : string(4 downto 2);
  signal bq : integer_vector(3 to 0);
  signal eml : time;
  signal ib : string(4 downto 2);
  signal kdlhulqmf : integer_vector(3 to 0);
  signal u : time;
begin
  wrs : entity work.ghxd
    port map (goqc => u, tpu => kdlhulqmf, emjqj => ib);
  f : entity work.ghxd
    port map (goqc => eml, tpu => bq, emjqj => olxqg);
  kvpuxi : entity work.ghxd
    port map (goqc => twjzos, tpu => hadxuxr, emjqj => ib);
  ttgeamdoiu : entity work.cing
    port map (jsuinh => vec, trubws => fccp, d => twjzos);
  
  -- Single-driven assignments
  ict <= ERROR;
end jwjoqi;



-- Seed after: 2461441978460886548,6176025545472508329
