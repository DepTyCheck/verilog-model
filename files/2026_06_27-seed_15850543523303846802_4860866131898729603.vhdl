-- Seed: 15850543523303846802,4860866131898729603

entity e is
  port (iia : in bit_vector(0 downto 2); mp : out integer; luoltpet : out real);
end e;

architecture xa of e is
  
begin
  
end xa;

entity cbc is
  port (qkgwkcl : in integer);
end cbc;

architecture y of cbc is
  signal rwlgzf : real;
  signal wphdpq : integer;
  signal ssyrvcf : bit_vector(0 downto 2);
  signal ojhst : real;
  signal vllgzcg : integer;
  signal slpej : bit_vector(0 downto 2);
  signal mcxfzhe : real;
  signal rzh : integer;
  signal enamug : bit_vector(0 downto 2);
  signal vhbtgsrp : real;
  signal uhwzn : integer;
  signal spxxygwkcd : bit_vector(0 downto 2);
begin
  ypymryi : entity work.e
    port map (iia => spxxygwkcd, mp => uhwzn, luoltpet => vhbtgsrp);
  wcnlojtg : entity work.e
    port map (iia => enamug, mp => rzh, luoltpet => mcxfzhe);
  fof : entity work.e
    port map (iia => slpej, mp => vllgzcg, luoltpet => ojhst);
  ocbx : entity work.e
    port map (iia => ssyrvcf, mp => wphdpq, luoltpet => rwlgzf);
  
  -- Single-driven assignments
  slpej <= (others => '0');
  enamug <= (others => '0');
  ssyrvcf <= (others => '0');
  spxxygwkcd <= (others => '0');
end y;

entity ysuvrcwbyx is
  port (bwdpxdguk : linkage integer_vector(2 downto 4));
end ysuvrcwbyx;

architecture abuxczc of ysuvrcwbyx is
  signal zthkf : real;
  signal ck : integer;
  signal fdww : real;
  signal b : integer;
  signal kw : bit_vector(0 downto 2);
begin
  jexcb : entity work.e
    port map (iia => kw, mp => b, luoltpet => fdww);
  bsxmeup : entity work.cbc
    port map (qkgwkcl => b);
  jvo : entity work.e
    port map (iia => kw, mp => ck, luoltpet => zthkf);
end abuxczc;



-- Seed after: 121845587502860796,4860866131898729603
