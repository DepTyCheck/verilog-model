-- Seed: 13747035418889124876,6176025545472508329

entity ttmthd is
  port (li : in integer; egbujtiq : linkage integer; belivo : buffer integer_vector(1 to 4));
end ttmthd;

architecture trpmzo of ttmthd is
  
begin
  -- Single-driven assignments
  belivo <= (8#20025#, 16#82#, 4213, 16#9467#);
end trpmzo;

entity nkmak is
  port (mfifgwn : inout integer; lrdpfuczr : in integer_vector(4 downto 0); vksnugf : linkage bit_vector(1 downto 2); kzh : in time_vector(0 downto 2));
end nkmak;

architecture ash of nkmak is
  signal uxukul : integer_vector(1 to 4);
  signal xlkhagxnbs : integer_vector(1 to 4);
  signal qanegw : integer;
begin
  h : entity work.ttmthd
    port map (li => mfifgwn, egbujtiq => qanegw, belivo => xlkhagxnbs);
  hbssbjwz : entity work.ttmthd
    port map (li => mfifgwn, egbujtiq => mfifgwn, belivo => uxukul);
end ash;

entity bfzqummn is
  port (jlbl : in boolean; hbdgbw : buffer boolean_vector(0 downto 3); wukzttcoo : out bit_vector(0 to 0));
end bfzqummn;

architecture cgelnnt of bfzqummn is
  signal tsjr : integer_vector(1 to 4);
  signal wmggedog : integer;
  signal cunuzuk : integer_vector(1 to 4);
  signal kbcafuiu : integer;
  signal euc : integer;
  signal mbddghn : bit_vector(1 downto 2);
  signal pes : integer;
  signal yfvxskdd : time_vector(0 downto 2);
  signal vgrkwrth : bit_vector(1 downto 2);
  signal jxhbujytu : integer_vector(4 downto 0);
  signal qddyzu : integer;
begin
  rxwkclrdyt : entity work.nkmak
    port map (mfifgwn => qddyzu, lrdpfuczr => jxhbujytu, vksnugf => vgrkwrth, kzh => yfvxskdd);
  skwrg : entity work.nkmak
    port map (mfifgwn => pes, lrdpfuczr => jxhbujytu, vksnugf => mbddghn, kzh => yfvxskdd);
  lunclz : entity work.ttmthd
    port map (li => euc, egbujtiq => kbcafuiu, belivo => cunuzuk);
  xdzvfxqahj : entity work.ttmthd
    port map (li => wmggedog, egbujtiq => euc, belivo => tsjr);
end cgelnnt;



-- Seed after: 2625531914152201652,6176025545472508329
