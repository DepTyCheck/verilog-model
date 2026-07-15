-- Seed: 5570309894678522147,2983771601630957889

use std.reflection.all;

entity wkg is
  port ( variable b : inout array_subtype_mirror_pt
  ; variable eibyqlgilb : inout protected_value_mirror_pt
  ; variable fibcd : inout physical_subtype_mirror_pt
  );
end wkg;

architecture si of wkg is
  
begin
  
end si;

use std.reflection.all;

entity lk is
  port ( variable uk : inout floating_value_mirror_pt
  ; awcrplrvbx : in character
  ; variable gj : inout access_value_mirror_pt
  ; variable gmoaawt : inout access_subtype_mirror_pt
  );
end lk;

use std.reflection.all;

architecture efbi of lk is
  shared variable dkjyzfkzo : physical_subtype_mirror_pt;
  shared variable xozxasm : protected_value_mirror_pt;
  shared variable xfj : array_subtype_mirror_pt;
  shared variable qzzwtahr : physical_subtype_mirror_pt;
  shared variable ortcdw : protected_value_mirror_pt;
  shared variable xpyjlzbyav : array_subtype_mirror_pt;
begin
  wysyugki : entity work.wkg
    port map (b => xpyjlzbyav, eibyqlgilb => ortcdw, fibcd => qzzwtahr);
  onlxuapca : entity work.wkg
    port map (b => xfj, eibyqlgilb => xozxasm, fibcd => dkjyzfkzo);
end efbi;

use std.reflection.all;

entity kfms is
  port (variable feftmmlpy : inout access_subtype_mirror_pt; znmdyvrjcr : buffer time; variable tiskk : inout file_subtype_mirror_pt);
end kfms;

use std.reflection.all;

architecture gawq of kfms is
  shared variable mdqoagn : physical_subtype_mirror_pt;
  shared variable gdccoopxd : protected_value_mirror_pt;
  shared variable zijkraqu : array_subtype_mirror_pt;
  shared variable dsywiqa : access_subtype_mirror_pt;
  shared variable eiui : access_value_mirror_pt;
  signal tswj : character;
  shared variable tqaijlfix : floating_value_mirror_pt;
begin
  hqrgfz : entity work.lk
    port map (uk => tqaijlfix, awcrplrvbx => tswj, gj => eiui, gmoaawt => dsywiqa);
  obfohre : entity work.wkg
    port map (b => zijkraqu, eibyqlgilb => gdccoopxd, fibcd => mdqoagn);
  
  -- Single-driven assignments
  znmdyvrjcr <= 0.0_3 ms;
end gawq;

entity ntygpf is
  port (fzeqfm : out time);
end ntygpf;

use std.reflection.all;

architecture xvx of ntygpf is
  shared variable eq : physical_subtype_mirror_pt;
  shared variable ogvju : protected_value_mirror_pt;
  shared variable dks : array_subtype_mirror_pt;
begin
  yafmomwj : entity work.wkg
    port map (b => dks, eibyqlgilb => ogvju, fibcd => eq);
  
  -- Single-driven assignments
  fzeqfm <= fzeqfm;
end xvx;



-- Seed after: 1214796351291752167,2983771601630957889
