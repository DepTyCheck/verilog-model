-- Seed: 7682104146221934551,3426943171337919827

entity hwo is
  port (dtqcbzirx : out time; jcl : inout time);
end hwo;

architecture wpw of hwo is
  
begin
  -- Single-driven assignments
  jcl <= dtqcbzirx;
  dtqcbzirx <= 4403.2_4 fs;
end wpw;

entity dzmtkc is
  port (adjoitbckx : linkage boolean; kimbrcjhma : inout boolean; lq : inout integer);
end dzmtkc;

architecture nktnd of dzmtkc is
  signal ib : time;
  signal wadselnoy : time;
  signal xobica : time;
  signal swejytebbi : time;
begin
  tea : entity work.hwo
    port map (dtqcbzirx => swejytebbi, jcl => xobica);
  uzh : entity work.hwo
    port map (dtqcbzirx => wadselnoy, jcl => ib);
  
  -- Single-driven assignments
  lq <= 0;
  kimbrcjhma <= TRUE;
end nktnd;

entity ghkdsne is
  port (ulf : inout integer_vector(4 downto 1));
end ghkdsne;

architecture gnwcxbll of ghkdsne is
  signal rvr : time;
  signal cmkvvf : time;
  signal xwxg : time;
  signal jpms : time;
  signal ef : time;
  signal rwgfolu : time;
  signal qkpxninjsk : time;
  signal ktns : time;
begin
  ha : entity work.hwo
    port map (dtqcbzirx => ktns, jcl => qkpxninjsk);
  gjbayjxjpc : entity work.hwo
    port map (dtqcbzirx => rwgfolu, jcl => ef);
  vgaoymgdhd : entity work.hwo
    port map (dtqcbzirx => jpms, jcl => xwxg);
  zkt : entity work.hwo
    port map (dtqcbzirx => cmkvvf, jcl => rvr);
  
  -- Single-driven assignments
  ulf <= ulf;
end gnwcxbll;

entity jrgvhsdxl is
  port (kcs : linkage time; wsafti : buffer real; ogxwx : buffer time; uhx : linkage boolean);
end jrgvhsdxl;

architecture wffowuhg of jrgvhsdxl is
  signal dhqumzxnr : integer_vector(4 downto 1);
  signal apjaafr : time;
  signal kuwucbgqf : time;
  signal kgjgay : time;
  signal caxmmtbip : integer_vector(4 downto 1);
begin
  fjqkxm : entity work.ghkdsne
    port map (ulf => caxmmtbip);
  arr : entity work.hwo
    port map (dtqcbzirx => kgjgay, jcl => kuwucbgqf);
  p : entity work.hwo
    port map (dtqcbzirx => apjaafr, jcl => ogxwx);
  gvptnkhso : entity work.ghkdsne
    port map (ulf => dhqumzxnr);
  
  -- Single-driven assignments
  wsafti <= wsafti;
end wffowuhg;



-- Seed after: 6000526647685130279,3426943171337919827
