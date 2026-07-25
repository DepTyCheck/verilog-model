-- Seed: 5087101021893217582,5306691039457971049

entity mdqjxtzrn is
  port (zjsiwm : in string(3 to 2); bdrnfarwoa : linkage bit_vector(4 to 0));
end mdqjxtzrn;

architecture jdemlxoa of mdqjxtzrn is
  
begin
  
end jdemlxoa;

entity wezizygya is
  port (ruxsisb : buffer boolean; q : buffer character; npm : out integer; zwzor : in bit);
end wezizygya;

architecture nsbgh of wezizygya is
  signal kgous : bit_vector(4 to 0);
  signal cul : string(3 to 2);
begin
  khiullne : entity work.mdqjxtzrn
    port map (zjsiwm => cul, bdrnfarwoa => kgous);
end nsbgh;

entity xaksub is
  port (yvywcfjdam : buffer bit);
end xaksub;

architecture nztil of xaksub is
  signal wdo : bit_vector(4 to 0);
  signal kuzxsdist : bit_vector(4 to 0);
  signal gk : bit_vector(4 to 0);
  signal czhnoukhuy : bit_vector(4 to 0);
  signal yhlyjr : string(3 to 2);
begin
  xm : entity work.mdqjxtzrn
    port map (zjsiwm => yhlyjr, bdrnfarwoa => czhnoukhuy);
  qh : entity work.mdqjxtzrn
    port map (zjsiwm => yhlyjr, bdrnfarwoa => gk);
  wgyazw : entity work.mdqjxtzrn
    port map (zjsiwm => yhlyjr, bdrnfarwoa => kuzxsdist);
  bgyhpmxdjg : entity work.mdqjxtzrn
    port map (zjsiwm => yhlyjr, bdrnfarwoa => wdo);
  
  -- Single-driven assignments
  yvywcfjdam <= '0';
end nztil;



-- Seed after: 3565012598916060128,5306691039457971049
