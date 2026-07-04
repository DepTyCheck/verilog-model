-- Seed: 13429238964567729573,6290177331721581829

use std.reflection.all;

entity iwtdcobic is
  port (xjedl : inout value_mirror; epcz : buffer real; sr : inout floating_value_mirror);
end iwtdcobic;

architecture are of iwtdcobic is
  
begin
  -- Single-driven assignments
  epcz <= epcz;
end are;

use std.reflection.all;

entity dnydmsxg is
  port (szevrzdjug : inout file_value_mirror);
end dnydmsxg;

use std.reflection.all;

architecture euokufwtno of dnydmsxg is
  shared variable mv : floating_value_mirror;
  signal dpkchjzlr : real;
  shared variable pevfxzapy : value_mirror;
  shared variable dunvvbsn : floating_value_mirror;
  signal znthttrflh : real;
  shared variable jcd : value_mirror;
  shared variable ltl : floating_value_mirror;
  signal ovbz : real;
  shared variable fpnbfp : value_mirror;
  shared variable twvgmfc : floating_value_mirror;
  signal qlxiqw : real;
  shared variable mrrzbbyse : value_mirror;
begin
  oubb : entity work.iwtdcobic
    port map (xjedl => mrrzbbyse, epcz => qlxiqw, sr => twvgmfc);
  mfqm : entity work.iwtdcobic
    port map (xjedl => fpnbfp, epcz => ovbz, sr => ltl);
  zxgdwazlgu : entity work.iwtdcobic
    port map (xjedl => jcd, epcz => znthttrflh, sr => dunvvbsn);
  slr : entity work.iwtdcobic
    port map (xjedl => pevfxzapy, epcz => dpkchjzlr, sr => mv);
end euokufwtno;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity jb is
  port (pwcsl : inout record_subtype_mirror; xjll : inout array_value_mirror; wnsvzha : out std_logic);
end jb;

use std.reflection.all;

architecture msdftq of jb is
  shared variable c : floating_value_mirror;
  signal u : real;
  shared variable jnrndxnsu : value_mirror;
  shared variable mpkdmx : floating_value_mirror;
  signal dvipzhlor : real;
  shared variable wtsgdajg : value_mirror;
  shared variable dmu : file_value_mirror;
  shared variable p : floating_value_mirror;
  signal wlndgxp : real;
  shared variable aakdalos : value_mirror;
begin
  pgkvibm : entity work.iwtdcobic
    port map (xjedl => aakdalos, epcz => wlndgxp, sr => p);
  eg : entity work.dnydmsxg
    port map (szevrzdjug => dmu);
  zxqndnt : entity work.iwtdcobic
    port map (xjedl => wtsgdajg, epcz => dvipzhlor, sr => mpkdmx);
  gwwdzrztlf : entity work.iwtdcobic
    port map (xjedl => jnrndxnsu, epcz => u, sr => c);
  
  -- Multi-driven assignments
  wnsvzha <= wnsvzha;
  wnsvzha <= 'U';
  wnsvzha <= wnsvzha;
end msdftq;



-- Seed after: 9001146794003923972,6290177331721581829
