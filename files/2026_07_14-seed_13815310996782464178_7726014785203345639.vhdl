-- Seed: 13815310996782464178,7726014785203345639

use std.reflection.all;

entity v is
  port (hchclk : out time; emblph : inout record_subtype_mirror; znhnrgbp : inout value_mirror; cfkxtue : inout protected_subtype_mirror);
end v;

architecture jfj of v is
  
begin
  -- Single-driven assignments
  hchclk <= hchclk;
end jfj;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity lm is
  port (d : in character; mpwapwfs : inout integer_subtype_mirror; kkbch : inout std_logic; lmdtrvnu : linkage std_logic_vector(2 to 1));
end lm;

use std.reflection.all;

architecture c of lm is
  shared variable bcznvpgi : protected_subtype_mirror;
  shared variable fjnjx : value_mirror;
  shared variable zfhoeoy : record_subtype_mirror;
  signal rzyijldna : time;
  shared variable tcvjmrjyil : protected_subtype_mirror;
  shared variable vxlrxyar : value_mirror;
  shared variable jpyhwq : record_subtype_mirror;
  signal sku : time;
  shared variable av : protected_subtype_mirror;
  shared variable blk : value_mirror;
  shared variable ffjozrna : record_subtype_mirror;
  signal drvhcqa : time;
begin
  czkfnkr : entity work.v
    port map (hchclk => drvhcqa, emblph => ffjozrna, znhnrgbp => blk, cfkxtue => av);
  ebig : entity work.v
    port map (hchclk => sku, emblph => jpyhwq, znhnrgbp => vxlrxyar, cfkxtue => tcvjmrjyil);
  mjmyylfiow : entity work.v
    port map (hchclk => rzyijldna, emblph => zfhoeoy, znhnrgbp => fjnjx, cfkxtue => bcznvpgi);
end c;

use std.reflection.all;

entity toqmgpr is
  port (ok : buffer boolean; kv : buffer integer_vector(3 to 3); mwmhdc : inout enumeration_subtype_mirror);
end toqmgpr;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture pzfw of toqmgpr is
  signal sbftqodesf : std_logic_vector(2 to 1);
  signal mifqdby : std_logic;
  shared variable izrsniy : integer_subtype_mirror;
  signal lcfpgrkhs : character;
  shared variable bxvk : protected_subtype_mirror;
  shared variable kipxogfri : value_mirror;
  shared variable szliubh : record_subtype_mirror;
  signal tlrmndcqb : time;
begin
  emeofhvrry : entity work.v
    port map (hchclk => tlrmndcqb, emblph => szliubh, znhnrgbp => kipxogfri, cfkxtue => bxvk);
  suuvsrdp : entity work.lm
    port map (d => lcfpgrkhs, mpwapwfs => izrsniy, kkbch => mifqdby, lmdtrvnu => sbftqodesf);
  
  -- Single-driven assignments
  lcfpgrkhs <= lcfpgrkhs;
  kv <= kv;
  ok <= ok;
  
  -- Multi-driven assignments
  mifqdby <= 'W';
  sbftqodesf <= (others => '0');
  sbftqodesf <= sbftqodesf;
  mifqdby <= mifqdby;
end pzfw;



-- Seed after: 14939837950183920075,7726014785203345639
