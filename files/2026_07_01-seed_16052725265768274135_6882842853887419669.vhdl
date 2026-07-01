-- Seed: 16052725265768274135,6882842853887419669

entity dfhvobl is
  port (lzbnmdju : inout character; jxtjaqeo : in time; mkavhsnhi : buffer string(1 to 3));
end dfhvobl;

architecture nfczamra of dfhvobl is
  
begin
  -- Single-driven assignments
  mkavhsnhi <= "kzu";
  lzbnmdju <= 'm';
end nfczamra;

entity ukiukg is
  port (hqzhgwr : inout boolean_vector(2 downto 3); polidfxko : linkage real; pwi : in bit_vector(0 downto 1); bd : out time);
end ukiukg;

architecture gytxar of ukiukg is
  signal hiccgavu : string(1 to 3);
  signal zvamnphycc : time;
  signal usymrw : character;
  signal qp : string(1 to 3);
  signal vvsovax : time;
  signal kly : character;
begin
  mfdjmubaj : entity work.dfhvobl
    port map (lzbnmdju => kly, jxtjaqeo => vvsovax, mkavhsnhi => qp);
  dz : entity work.dfhvobl
    port map (lzbnmdju => usymrw, jxtjaqeo => zvamnphycc, mkavhsnhi => hiccgavu);
  
  -- Single-driven assignments
  bd <= 2#01.11# ns;
  hqzhgwr <= (others => TRUE);
end gytxar;

entity vpsi is
  port (pspxfl : out time_vector(0 to 1));
end vpsi;

architecture jaxikul of vpsi is
  signal mdio : string(1 to 3);
  signal oakclbc : time;
  signal kvlnfjxclj : character;
  signal x : time;
  signal mavo : bit_vector(0 downto 1);
  signal gfpxbj : real;
  signal ggdcquz : boolean_vector(2 downto 3);
begin
  qqorovnkl : entity work.ukiukg
    port map (hqzhgwr => ggdcquz, polidfxko => gfpxbj, pwi => mavo, bd => x);
  rn : entity work.dfhvobl
    port map (lzbnmdju => kvlnfjxclj, jxtjaqeo => oakclbc, mkavhsnhi => mdio);
  
  -- Single-driven assignments
  pspxfl <= (002.4_2_2 ms, 16#E_6_2_E_9.F_8# ps);
end jaxikul;

entity dr is
  port (iuvvsktbno : buffer real_vector(2 to 3); boap : inout time; d : out time);
end dr;

architecture hu of dr is
  signal gvngemuf : string(1 to 3);
  signal b : character;
  signal kbit : string(1 to 3);
  signal phpclls : time;
  signal qkjf : character;
  signal ef : time;
  signal zovywxoum : bit_vector(0 downto 1);
  signal cuqv : real;
  signal vnswrvvx : boolean_vector(2 downto 3);
begin
  s : entity work.ukiukg
    port map (hqzhgwr => vnswrvvx, polidfxko => cuqv, pwi => zovywxoum, bd => ef);
  gokq : entity work.dfhvobl
    port map (lzbnmdju => qkjf, jxtjaqeo => phpclls, mkavhsnhi => kbit);
  hnmaij : entity work.dfhvobl
    port map (lzbnmdju => b, jxtjaqeo => d, mkavhsnhi => gvngemuf);
  
  -- Single-driven assignments
  iuvvsktbno <= (0324.3002, 3_3_1.21);
  d <= 1_1_1 ns;
  boap <= 8#5_4_2.1_4_7# ps;
  zovywxoum <= (others => '0');
end hu;



-- Seed after: 3403434185111904301,6882842853887419669
