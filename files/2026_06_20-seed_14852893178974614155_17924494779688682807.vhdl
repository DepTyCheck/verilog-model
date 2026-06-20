-- Seed: 14852893178974614155,17924494779688682807

entity jdkanwi is
  port (qmnom : in real; bstnyicjit : buffer integer; wrcpbd : inout real);
end jdkanwi;

architecture swnkgn of jdkanwi is
  
begin
  -- Single-driven assignments
  wrcpbd <= 8#2.7560#;
  bstnyicjit <= 2#0_1_1_0_1#;
end swnkgn;

entity bvwlrvqjku is
  port (zlen : in integer; c : out time);
end bvwlrvqjku;

architecture fmhmml of bvwlrvqjku is
  signal rvzjpxclzw : integer;
  signal lxrm : real;
  signal raaq : real;
  signal hcsqys : integer;
  signal vyagml : real;
  signal wypvlyqyh : integer;
  signal jk : real;
  signal bxaltmfqrf : integer;
  signal mufqueiusc : real;
begin
  vicwkv : entity work.jdkanwi
    port map (qmnom => mufqueiusc, bstnyicjit => bxaltmfqrf, wrcpbd => mufqueiusc);
  naqdrvy : entity work.jdkanwi
    port map (qmnom => jk, bstnyicjit => wypvlyqyh, wrcpbd => vyagml);
  ewfdl : entity work.jdkanwi
    port map (qmnom => jk, bstnyicjit => hcsqys, wrcpbd => raaq);
  qafaix : entity work.jdkanwi
    port map (qmnom => lxrm, bstnyicjit => rvzjpxclzw, wrcpbd => jk);
end fmhmml;



-- Seed after: 6927125207073721172,17924494779688682807
