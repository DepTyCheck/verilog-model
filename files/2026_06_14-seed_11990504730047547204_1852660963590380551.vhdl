-- Seed: 11990504730047547204,1852660963590380551



entity dti is
  port (mfrobaykks : out time_vector(1 downto 0); efmfcfjy : in bit; nwym : inout real; uhxkgm : linkage bit_vector(4 to 3));
end dti;



architecture takdlplhal of dti is
  
begin
  
end takdlplhal;



entity iw is
  port (juy : out boolean; netqhje : out boolean_vector(4 downto 3));
end iw;



architecture ckfhluap of iw is
  signal lfmbqew : bit_vector(4 to 3);
  signal gum : real;
  signal xrwpdiz : time_vector(1 downto 0);
  signal bucixtsd : real;
  signal obtzbffgn : bit;
  signal wdeqyr : time_vector(1 downto 0);
  signal gw : bit_vector(4 to 3);
  signal us : real;
  signal phkbhktd : time_vector(1 downto 0);
  signal iqarfprnb : bit_vector(4 to 3);
  signal p : real;
  signal zgnqbbuot : bit;
  signal ecktqvvfx : time_vector(1 downto 0);
begin
  jgv : entity work.dti
    port map (mfrobaykks => ecktqvvfx, efmfcfjy => zgnqbbuot, nwym => p, uhxkgm => iqarfprnb);
  ocnqss : entity work.dti
    port map (mfrobaykks => phkbhktd, efmfcfjy => zgnqbbuot, nwym => us, uhxkgm => gw);
  azfkl : entity work.dti
    port map (mfrobaykks => wdeqyr, efmfcfjy => obtzbffgn, nwym => bucixtsd, uhxkgm => iqarfprnb);
  wn : entity work.dti
    port map (mfrobaykks => xrwpdiz, efmfcfjy => zgnqbbuot, nwym => gum, uhxkgm => lfmbqew);
end ckfhluap;



entity ap is
  port (vhuoh : inout time);
end ap;



architecture oqclf of ap is
  signal kkmbpxk : bit_vector(4 to 3);
  signal h : real;
  signal f : bit;
  signal temsesp : time_vector(1 downto 0);
begin
  tibxfpxtkg : entity work.dti
    port map (mfrobaykks => temsesp, efmfcfjy => f, nwym => h, uhxkgm => kkmbpxk);
end oqclf;



entity hcvqdudfry is
  port (xkkhxlu : out time_vector(1 downto 4); tbmhujls : buffer severity_level; zwvqith : out time);
end hcvqdudfry;



architecture codxui of hcvqdudfry is
  signal mwtmymt : bit_vector(4 to 3);
  signal xo : real;
  signal hcwbzw : time_vector(1 downto 0);
  signal xbxenyv : boolean_vector(4 downto 3);
  signal hnxovqv : boolean;
  signal rku : bit_vector(4 to 3);
  signal yo : real;
  signal lbwtrrsuo : bit;
  signal kkeqnw : time_vector(1 downto 0);
begin
  kx : entity work.dti
    port map (mfrobaykks => kkeqnw, efmfcfjy => lbwtrrsuo, nwym => yo, uhxkgm => rku);
  csrdhwy : entity work.iw
    port map (juy => hnxovqv, netqhje => xbxenyv);
  zei : entity work.ap
    port map (vhuoh => zwvqith);
  anuflbmc : entity work.dti
    port map (mfrobaykks => hcwbzw, efmfcfjy => lbwtrrsuo, nwym => xo, uhxkgm => mwtmymt);
end codxui;



-- Seed after: 13108275119786246017,1852660963590380551
