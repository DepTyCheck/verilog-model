-- Seed: 4696826471491465911,3042374792655995433

entity jqhmpx is
  port (cv : inout boolean_vector(0 downto 0); fkyp : buffer time; qk : out time);
end jqhmpx;

architecture e of jqhmpx is
  
begin
  -- Single-driven assignments
  qk <= fkyp;
  cv <= cv;
  fkyp <= qk;
end e;

entity wykesgvvuo is
  port (s : in integer; fqoecbdl : in real_vector(0 downto 1));
end wykesgvvuo;

architecture eopdv of wykesgvvuo is
  signal imppmkjx : time;
  signal pppxuvafzh : time;
  signal lahpfsxhi : boolean_vector(0 downto 0);
  signal ahtowcstio : time;
  signal uiwqnt : time;
  signal qepaby : boolean_vector(0 downto 0);
  signal tqrzv : time;
  signal hinaxvi : time;
  signal jhuryf : boolean_vector(0 downto 0);
  signal khkhc : time;
  signal saaf : time;
  signal sjb : boolean_vector(0 downto 0);
begin
  v : entity work.jqhmpx
    port map (cv => sjb, fkyp => saaf, qk => khkhc);
  ionxpzjed : entity work.jqhmpx
    port map (cv => jhuryf, fkyp => hinaxvi, qk => tqrzv);
  lizknv : entity work.jqhmpx
    port map (cv => qepaby, fkyp => uiwqnt, qk => ahtowcstio);
  zbed : entity work.jqhmpx
    port map (cv => lahpfsxhi, fkyp => pppxuvafzh, qk => imppmkjx);
end eopdv;



-- Seed after: 17148553702449877689,3042374792655995433
