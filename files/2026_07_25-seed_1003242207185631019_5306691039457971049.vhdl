-- Seed: 1003242207185631019,5306691039457971049

entity rgcnxyvft is
  port (ahdy : buffer bit_vector(4 to 3); omjbyu : inout time);
end rgcnxyvft;

architecture fnnsrxv of rgcnxyvft is
  
begin
  -- Single-driven assignments
  ahdy <= ahdy;
end fnnsrxv;

entity ni is
  port (fpdzmxrjg : linkage integer_vector(4 to 1); pj : inout time; lroyuwx : out time);
end ni;

architecture jsnczmd of ni is
  signal tflm : bit_vector(4 to 3);
  signal ilrhrlpqdc : time;
  signal puelohvgh : bit_vector(4 to 3);
begin
  q : entity work.rgcnxyvft
    port map (ahdy => puelohvgh, omjbyu => ilrhrlpqdc);
  tqjmuat : entity work.rgcnxyvft
    port map (ahdy => tflm, omjbyu => lroyuwx);
  
  -- Single-driven assignments
  pj <= 8#6_4# ms;
end jsnczmd;

entity gevsruc is
  port (dgvs : linkage integer; iufapvutmi : linkage real);
end gevsruc;

architecture xenkzajty of gevsruc is
  signal wfdtowi : time;
  signal te : time;
  signal vo : integer_vector(4 to 1);
  signal lmocw : time;
  signal j : bit_vector(4 to 3);
begin
  cfmmvmxqs : entity work.rgcnxyvft
    port map (ahdy => j, omjbyu => lmocw);
  wurnzlonh : entity work.ni
    port map (fpdzmxrjg => vo, pj => te, lroyuwx => wfdtowi);
end xenkzajty;



-- Seed after: 13407538709816850635,5306691039457971049
