-- Seed: 2474261843967433194,14426950258250697445

use std.reflection.all;

entity sgy is
  port (rzhlezls : inout enumeration_value_mirror);
end sgy;

architecture xbamplx of sgy is
  
begin
  
end xbamplx;

entity qvjtglrwnc is
  port (uyet : linkage character);
end qvjtglrwnc;

use std.reflection.all;

architecture ridwy of qvjtglrwnc is
  shared variable zoubhskngh : enumeration_value_mirror;
  shared variable syeenrbc : enumeration_value_mirror;
  shared variable vowzd : enumeration_value_mirror;
  shared variable iuhl : enumeration_value_mirror;
begin
  xej : entity work.sgy
    port map (rzhlezls => iuhl);
  ro : entity work.sgy
    port map (rzhlezls => vowzd);
  ywl : entity work.sgy
    port map (rzhlezls => syeenrbc);
  dqxjipws : entity work.sgy
    port map (rzhlezls => zoubhskngh);
end ridwy;

use std.reflection.all;

entity rsikixynv is
  port (mcae : buffer boolean_vector(3 downto 3); rwsaaaoa : inout file_value_mirror; nzubjmvzw : inout character);
end rsikixynv;

use std.reflection.all;

architecture k of rsikixynv is
  shared variable bbjvw : enumeration_value_mirror;
  shared variable ck : enumeration_value_mirror;
  signal mxzzctg : character;
begin
  vos : entity work.qvjtglrwnc
    port map (uyet => mxzzctg);
  nkdoet : entity work.sgy
    port map (rzhlezls => ck);
  rtp : entity work.sgy
    port map (rzhlezls => bbjvw);
  
  -- Single-driven assignments
  mcae <= (others => TRUE);
  nzubjmvzw <= 'i';
end k;



-- Seed after: 1036681292883052040,14426950258250697445
