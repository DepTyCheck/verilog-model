-- Seed: 16344254394583696318,4404421571376382767

entity mpffu is
  port (gor : linkage string(1 downto 5); mwgnstknyx : buffer character);
end mpffu;

architecture fvr of mpffu is
  
begin
  -- Single-driven assignments
  mwgnstknyx <= 'r';
end fvr;

entity hhmxlqz is
  port (qde : inout real_vector(4 downto 2); qbipo : inout bit);
end hhmxlqz;

architecture bqv of hhmxlqz is
  signal r : character;
  signal ssyayd : string(1 downto 5);
  signal jkylsp : character;
  signal xadkh : string(1 downto 5);
  signal nbri : character;
  signal ipmx : string(1 downto 5);
  signal scc : character;
  signal c : string(1 downto 5);
begin
  agsvuo : entity work.mpffu
    port map (gor => c, mwgnstknyx => scc);
  nxdvestqjo : entity work.mpffu
    port map (gor => ipmx, mwgnstknyx => nbri);
  sirf : entity work.mpffu
    port map (gor => xadkh, mwgnstknyx => jkylsp);
  d : entity work.mpffu
    port map (gor => ssyayd, mwgnstknyx => r);
end bqv;



-- Seed after: 7648728782265824022,4404421571376382767
