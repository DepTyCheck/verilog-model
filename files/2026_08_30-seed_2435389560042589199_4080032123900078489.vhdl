-- Seed: 2435389560042589199,4080032123900078489

entity u is
  port (cxk : buffer string(2 downto 2); yhwjukxem : out bit_vector(3 to 2); cimtbkzjq : inout time; qwtww : out integer);
end u;

architecture jzpubh of u is
  
begin
  -- Single-driven assignments
  qwtww <= 16#0_6#;
  cxk <= "n";
  yhwjukxem <= yhwjukxem;
  cimtbkzjq <= cimtbkzjq;
end jzpubh;

entity rnashzkbd is
  port (utnpg : out severity_level);
end rnashzkbd;

architecture f of rnashzkbd is
  signal zavbo : integer;
  signal eowgwiqme : time;
  signal bnwkylv : bit_vector(3 to 2);
  signal wgbr : string(2 downto 2);
  signal wcjohw : integer;
  signal w : time;
  signal ualqyff : bit_vector(3 to 2);
  signal dpcu : string(2 downto 2);
  signal wzds : integer;
  signal kppisb : time;
  signal ey : bit_vector(3 to 2);
  signal eim : string(2 downto 2);
  signal xlykbg : integer;
  signal yqdzr : time;
  signal ktavs : bit_vector(3 to 2);
  signal ysfowo : string(2 downto 2);
begin
  alp : entity work.u
    port map (cxk => ysfowo, yhwjukxem => ktavs, cimtbkzjq => yqdzr, qwtww => xlykbg);
  p : entity work.u
    port map (cxk => eim, yhwjukxem => ey, cimtbkzjq => kppisb, qwtww => wzds);
  yekhi : entity work.u
    port map (cxk => dpcu, yhwjukxem => ualqyff, cimtbkzjq => w, qwtww => wcjohw);
  jjborj : entity work.u
    port map (cxk => wgbr, yhwjukxem => bnwkylv, cimtbkzjq => eowgwiqme, qwtww => zavbo);
  
  -- Single-driven assignments
  utnpg <= NOTE;
end f;



-- Seed after: 8966200656037463930,4080032123900078489
