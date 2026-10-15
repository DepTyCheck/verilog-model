-- Seed: 16672478761686788836,6377581586043967497

entity s is
  port (dc : out real);
end s;

architecture vhzlpqinr of s is
  
begin
  -- Single-driven assignments
  dc <= 16#DBA68.6#;
end vhzlpqinr;

entity xunvy is
  port (vrvo : inout bit_vector(4 to 0));
end xunvy;

architecture olhslfz of xunvy is
  signal k : real;
  signal smwuxxaykx : real;
  signal qajp : real;
  signal xyfdzbpb : real;
begin
  woqqevkgr : entity work.s
    port map (dc => xyfdzbpb);
  yfvcuss : entity work.s
    port map (dc => qajp);
  qnxwfjzffc : entity work.s
    port map (dc => smwuxxaykx);
  uqhwtsljf : entity work.s
    port map (dc => k);
end olhslfz;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (ux : in real; ktawkwe : buffer std_logic; qlfcidaso : buffer std_logic);
end m;

architecture gfxih of m is
  signal a : bit_vector(4 to 0);
  signal mfzgcmq : real;
  signal mnrji : real;
begin
  zsgpk : entity work.s
    port map (dc => mnrji);
  yr : entity work.s
    port map (dc => mfzgcmq);
  jqgy : entity work.xunvy
    port map (vrvo => a);
end gfxih;



-- Seed after: 7101423942475630328,6377581586043967497
