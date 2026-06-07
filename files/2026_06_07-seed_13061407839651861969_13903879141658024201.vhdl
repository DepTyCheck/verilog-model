-- Seed: 13061407839651861969,13903879141658024201



entity szlfsy is
  port (rgddqd : out integer; lg : in time; zd : linkage integer; id : out real_vector(2 downto 1));
end szlfsy;



architecture fgzw of szlfsy is
  
begin
  
end fgzw;



entity l is
  port (imbayxqx : buffer integer; m : in time);
end l;



architecture dxtwfzqv of l is
  signal qyoreo : real_vector(2 downto 1);
  signal ufxsjmrv : time;
  signal c : integer;
begin
  uovmlfniqh : entity work.szlfsy
    port map (rgddqd => c, lg => ufxsjmrv, zd => imbayxqx, id => qyoreo);
end dxtwfzqv;



entity hkozchpnu is
  port (qzojxmqjo : in bit);
end hkozchpnu;



architecture ggardyx of hkozchpnu is
  signal vhn : real_vector(2 downto 1);
  signal m : time;
  signal klzrstpjy : real_vector(2 downto 1);
  signal bh : integer;
  signal blttxpkhp : integer;
  signal q : real_vector(2 downto 1);
  signal kt : integer;
  signal oxtxvk : time;
  signal mbpmmh : integer;
begin
  pbxa : entity work.l
    port map (imbayxqx => mbpmmh, m => oxtxvk);
  mk : entity work.szlfsy
    port map (rgddqd => kt, lg => oxtxvk, zd => kt, id => q);
  x : entity work.szlfsy
    port map (rgddqd => blttxpkhp, lg => oxtxvk, zd => bh, id => klzrstpjy);
  ot : entity work.szlfsy
    port map (rgddqd => bh, lg => m, zd => mbpmmh, id => vhn);
end ggardyx;



-- Seed after: 445083901206008936,13903879141658024201
