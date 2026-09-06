-- Seed: 17299505264080928734,14094562573555574003

entity lglau is
  port (bjtmp : inout time; u : in time_vector(4 to 4));
end lglau;

architecture sljavpceb of lglau is
  
begin
  -- Single-driven assignments
  bjtmp <= bjtmp;
end sljavpceb;

entity l is
  port (xac : in bit_vector(4 to 4); afkoamdj : in integer; xidc : out integer_vector(3 downto 4));
end l;

architecture flod of l is
  
begin
  -- Single-driven assignments
  xidc <= xidc;
end flod;

entity bkxz is
  port (hgk : buffer bit_vector(2 to 0));
end bkxz;

architecture nunmtgunjb of bkxz is
  signal pmsrcgysr : time_vector(4 to 4);
  signal pndhdi : time;
  signal nwc : integer_vector(3 downto 4);
  signal ulaxhayox : integer;
  signal iszertwha : bit_vector(4 to 4);
begin
  shu : entity work.l
    port map (xac => iszertwha, afkoamdj => ulaxhayox, xidc => nwc);
  un : entity work.lglau
    port map (bjtmp => pndhdi, u => pmsrcgysr);
  
  -- Single-driven assignments
  ulaxhayox <= ulaxhayox;
  hgk <= (others => '0');
  iszertwha <= (others => '0');
  pmsrcgysr <= (others => 8#615# fs);
end nunmtgunjb;



-- Seed after: 6166783544440922286,14094562573555574003
