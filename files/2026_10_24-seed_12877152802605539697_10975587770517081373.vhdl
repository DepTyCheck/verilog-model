-- Seed: 12877152802605539697,10975587770517081373

entity n is
  port (ukvdjej : inout integer; yvsgnacavl : buffer integer);
end n;

architecture q of n is
  
begin
  -- Single-driven assignments
  yvsgnacavl <= ukvdjej;
  ukvdjej <= yvsgnacavl;
end q;

entity uq is
  port (qfqwjiod : out integer; hkgyhpdl : out boolean);
end uq;

architecture qxyvmv of uq is
  signal dpoi : integer;
  signal iwkfm : integer;
  signal wnlxxrju : integer;
begin
  ornw : entity work.n
    port map (ukvdjej => wnlxxrju, yvsgnacavl => qfqwjiod);
  zc : entity work.n
    port map (ukvdjej => iwkfm, yvsgnacavl => dpoi);
  
  -- Single-driven assignments
  hkgyhpdl <= TRUE;
end qxyvmv;

entity rirttthqd is
  port (yuiovgpsr : in real);
end rirttthqd;

architecture ebctdb of rirttthqd is
  
begin
  
end ebctdb;



-- Seed after: 8265039684912299835,10975587770517081373
