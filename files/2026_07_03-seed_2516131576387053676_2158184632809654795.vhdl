-- Seed: 2516131576387053676,2158184632809654795

use std.reflection.all;

entity hlum is
  port (nexbhdg : out boolean; ukxwrhshzv : inout integer_value_mirror; bpxne : inout integer);
end hlum;

architecture nfqn of hlum is
  
begin
  -- Single-driven assignments
  bpxne <= 16#6_6#;
  nexbhdg <= FALSE;
end nfqn;

use std.reflection.all;

entity hteuyjfmeb is
  port (ubra : inout physical_value_mirror);
end hteuyjfmeb;

use std.reflection.all;

architecture bfyzcziz of hteuyjfmeb is
  signal ozndp : integer;
  shared variable fq : integer_value_mirror;
  signal ah : boolean;
  signal psp : integer;
  shared variable ngt : integer_value_mirror;
  signal ikzzrqd : boolean;
  signal bhqoneg : integer;
  shared variable bzanz : integer_value_mirror;
  signal nzxxqfc : boolean;
  signal r : integer;
  shared variable i : integer_value_mirror;
  signal x : boolean;
begin
  tqnqau : entity work.hlum
    port map (nexbhdg => x, ukxwrhshzv => i, bpxne => r);
  ycrzo : entity work.hlum
    port map (nexbhdg => nzxxqfc, ukxwrhshzv => bzanz, bpxne => bhqoneg);
  hegps : entity work.hlum
    port map (nexbhdg => ikzzrqd, ukxwrhshzv => ngt, bpxne => psp);
  h : entity work.hlum
    port map (nexbhdg => ah, ukxwrhshzv => fq, bpxne => ozndp);
end bfyzcziz;

use std.reflection.all;

entity olgtbraobg is
  port (t : inout access_subtype_mirror);
end olgtbraobg;

use std.reflection.all;

architecture qmr of olgtbraobg is
  shared variable q : physical_value_mirror;
  shared variable ysr : physical_value_mirror;
begin
  yosvhk : entity work.hteuyjfmeb
    port map (ubra => ysr);
  m : entity work.hteuyjfmeb
    port map (ubra => q);
end qmr;



-- Seed after: 6465680774954739992,2158184632809654795
