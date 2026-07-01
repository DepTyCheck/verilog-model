-- Seed: 2314217821922734412,6882842853887419669

entity ku is
  port (chq : in string(3 to 4); zjsrscd : inout string(4 downto 4));
end ku;

architecture nfhmuxen of ku is
  
begin
  -- Single-driven assignments
  zjsrscd <= "o";
end nfhmuxen;

entity cjkvvucdg is
  port (x : inout severity_level);
end cjkvvucdg;

architecture vbdcl of cjkvvucdg is
  signal otuinfij : string(4 downto 4);
  signal wrfq : string(3 to 4);
  signal garsnoh : string(4 downto 4);
  signal zngyc : string(4 downto 4);
  signal f : string(3 to 4);
begin
  kcfelsaqi : entity work.ku
    port map (chq => f, zjsrscd => zngyc);
  lkaqzncq : entity work.ku
    port map (chq => f, zjsrscd => garsnoh);
  tkd : entity work.ku
    port map (chq => wrfq, zjsrscd => otuinfij);
  
  -- Single-driven assignments
  f <= ('m', 't');
  wrfq <= ('u', 'o');
  x <= FAILURE;
end vbdcl;



-- Seed after: 92761786819308390,6882842853887419669
