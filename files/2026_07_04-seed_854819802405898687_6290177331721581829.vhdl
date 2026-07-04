-- Seed: 854819802405898687,6290177331721581829

use std.reflection.all;

entity s is
  port (eksg : inout floating_value_mirror; z : buffer integer; bpvjkxtjzj : inout subtype_mirror);
end s;

architecture ead of s is
  
begin
  -- Single-driven assignments
  z <= z;
end ead;

use std.reflection.all;

entity paa is
  port (stfksa : linkage real; xeknv : in boolean; flnhe : inout floating_subtype_mirror);
end paa;

use std.reflection.all;

architecture ik of paa is
  shared variable hodmtyc : subtype_mirror;
  signal ysoi : integer;
  shared variable juvyi : floating_value_mirror;
begin
  zmnjwhwbw : entity work.s
    port map (eksg => juvyi, z => ysoi, bpvjkxtjzj => hodmtyc);
end ik;

entity cepig is
  port (fkfarncsx : in real_vector(3 to 4));
end cepig;

use std.reflection.all;

architecture bqaqu of cepig is
  shared variable kkxhsjef : subtype_mirror;
  signal f : integer;
  shared variable gklvo : floating_value_mirror;
  shared variable ezzphcdkcf : floating_subtype_mirror;
  signal cb : boolean;
  signal wpvxie : real;
  shared variable wwqzpxtck : floating_subtype_mirror;
  signal ebn : boolean;
  signal rmrapq : real;
  shared variable ht : subtype_mirror;
  signal yz : integer;
  shared variable gch : floating_value_mirror;
begin
  jtbrgs : entity work.s
    port map (eksg => gch, z => yz, bpvjkxtjzj => ht);
  towerlofn : entity work.paa
    port map (stfksa => rmrapq, xeknv => ebn, flnhe => wwqzpxtck);
  ku : entity work.paa
    port map (stfksa => wpvxie, xeknv => cb, flnhe => ezzphcdkcf);
  sfvi : entity work.s
    port map (eksg => gklvo, z => f, bpvjkxtjzj => kkxhsjef);
  
  -- Single-driven assignments
  cb <= cb;
  ebn <= TRUE;
end bqaqu;



-- Seed after: 15579351944468292904,6290177331721581829
